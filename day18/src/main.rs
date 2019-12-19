
use {
    std::{
        collections::{HashMap, HashSet},
        mem::swap,
    },
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Tile {
    Start,
    Clear,
    Key(char),
    Door(char)
}

type NodeID = u32; //std::num::NonZeroU32;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Edge {
    pub min: NodeID,
    pub max: NodeID,
    pub weight: i32,
}

impl Edge {
    fn new(a: NodeID, b: NodeID, weight: i32) -> Edge {
        Edge {
            min: std::cmp::min(a, b),
            max: std::cmp::max(a, b),
            weight,
        }
    }
}

type Nodes = HashMap<NodeID, Tile>;
type Edges = HashSet<Edge>;

struct AdjNode {
    pub tile:      Tile,
    pub neighbors: HashSet<(NodeID, i32)>
}

type Adjs  = HashMap<NodeID, AdjNode>;

fn extract_graph(lines: &[&str]) -> (Nodes, Edges, NodeID) {
    let mut nodes = Nodes::new();
    let mut edges = Edges::new();
    let mut start = None;

    let mut id = 0;

    let mut prev_row: Vec<Option<NodeID>> = Vec::new();

    for line in lines.iter() {
        let mut this_row = Vec::new();

        for (ch, above) in line.chars().zip(prev_row.into_iter().chain(std::iter::repeat(None))) {
            let tile = match ch {
                '#' => {
                    this_row.push(None);
                    continue;
                }
                '.' => Tile::Clear,
                c if c.is_ascii_lowercase() => Tile::Key(c),
                c if c.is_ascii_uppercase() => Tile::Door(c),
                '@' => {
                    assert!(start.is_none());
                    start = Some(id);
                    Tile::Start
                }
                _ => panic!()
            };

            nodes.insert(id, tile);

            if !this_row.is_empty() && this_row.last().unwrap().is_some() {
                edges.insert(Edge::new(id, id - 1, 1));
            }

            if let Some(above_id) = above {
                edges.insert(Edge::new(id, above_id, 1));
            }

            id += 1;
            this_row.push(Some(id));
        }

        prev_row = this_row;
    }

    (nodes, edges, start.unwrap())
}

fn prune_graph(mut nodes: Nodes, mut edges: Edges) -> (Nodes, Edges) {
    let mut nodes_temp: Nodes = HashMap::with_capacity(nodes.len());
    let mut edges_temp: Edges = HashSet::with_capacity(edges.len());

    let mut degrees: HashMap<NodeID, usize> = HashMap::with_capacity(nodes.len());
    let mut dead_ends: HashSet<NodeID> = HashSet::with_capacity(nodes.len());

    loop {
        degrees.clear();
        degrees.extend(nodes
            .iter()
            .filter_map(|(id, tile)| match tile {
                Tile::Clear => Some((*id, 0)),
                _           => None
            })
        );

        for edge in edges.iter() {
            if let Some(deg) = degrees.get_mut(&edge.min) { *deg += 1; }
            if let Some(deg) = degrees.get_mut(&edge.max) { *deg += 1; }
        }

        dead_ends.clear();
        dead_ends.extend(degrees
            .iter()
            .filter(|(id, degree)| **degree < 2)
            .map(|(id, _)| *id)
        );

        if dead_ends.is_empty() {
            break;
        }

        edges_temp.clear();
        edges_temp.extend(edges.iter()
            .copied()
            .filter(|edge| !dead_ends.contains(&edge.min) && !dead_ends.contains(&edge.max))
        );
        swap(&mut edges, &mut edges_temp);

        nodes_temp.clear();
        nodes_temp.extend(nodes.iter()
            .filter(|(id, _)| !dead_ends.contains(&id))
            .map(|(id, data)| (*id, *data))
        );
        swap(&mut nodes, &mut nodes_temp);
    }

    (nodes, edges)
}

fn simplify_graph(mut nodes: Nodes, mut edges: Edges) -> (Nodes, Edges) {
    // remove uninteresting nodes - makes merging edges easier
    nodes = nodes.into_iter()
        .filter_map(|(id, tile)| match tile {
            Tile::Clear => None,
            _           => Some((id, tile))
        })
        .collect();

    for edge in edges {

    }

    (nodes, edges)
}

fn to_adjacency(nodes: Nodes, edges: Edges) -> Adjs {
    let mut adjs: Adjs = nodes.into_iter()
        .map(|(id, tile)| (id, AdjNode { tile: tile, neighbors: HashSet::new() }))
        .collect();

    for edge in edges.into_iter() {
        adjs.get_mut(&edge.min).unwrap().neighbors.insert((edge.max, edge.weight));
        adjs.get_mut(&edge.max).unwrap().neighbors.insert((edge.min, edge.weight));
    }

    adjs
}

#[derive(Clone, Copy, Debug)]
struct KeyBits(u32);

impl KeyBits {
    fn key_bit(key: char) -> u32 {
        let key = key.to_ascii_lowercase();
        match key {
            'a'..='z' => 1 << (u32::from(key) - u32::from('a')),
            _         => panic!("bad key {}", key)
        }
    }

    fn none() -> KeyBits { KeyBits(0) }

    fn with(self, key: char) -> KeyBits {
        KeyBits(self.0 | Self::key_bit(key))
    }

    fn has(self, key: char) -> bool {
        self.0 & Self::key_bit(key) != 0
    }

    fn has_all(self) -> bool {
        self.0 == 1 << 26 - 1
    }
}

fn search(
    adjs: &Adjs,
    keys: KeyBits,
    cur:  NodeID,
    prev: Option<NodeID>,
    cost: i32,
    best: &mut i32,
    ub:   i32)
{
    dbg!(cur, *best);

    let node = &adjs[&cur];

    let keys = match node.tile {
        Tile::Key(ch) => keys.with(ch),
        Tile::Door(ch) if !keys.has(ch) => { return; }
        _ => keys
    };

    if keys.has_all() {
        *best = std::cmp::min(*best, cost);
        return;
    }

    for (neighbor, delta) in node.neighbors.iter() {
        if cost + delta > ub {
            continue;
        }

        // don't backtrack if we have a choice
        if let Some(prev_id) = prev {
            if node.neighbors.len() > 1 && *neighbor == prev_id {
                continue;
            }
        }

        // don't bother continuing this path if we have a better one
        let new_cost = cost + delta;
        if *best <= new_cost {
            continue;
        }

        search(adjs, keys, *neighbor, Some(cur), new_cost, best, ub);
    }
}

fn shortest_tour(adjs: &Adjs, start: NodeID, upper_bound: i32) -> i32 {
    let mut best = std::i32::MAX;
    search(adjs, KeyBits::none(), start, None, 0, &mut best, upper_bound);
    best
}

fn main() {
    let input = include_str!("../input");
    let lines: Vec<&str> = input.lines().collect();
    let (nodes, edges, start) = extract_graph(&lines);
    let (nodes, edges) = prune_graph(nodes, edges);
    let upper_bound: i32 = edges.iter().map(|edge| edge.weight).sum();
    let adjs = to_adjacency(nodes, edges);
    let len = shortest_tour(&adjs, start, upper_bound);
}

