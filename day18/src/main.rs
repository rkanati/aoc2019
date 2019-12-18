
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

fn extract_graph(lines: &[&str]) -> (Nodes, Edges) {
    let mut nodes = Nodes::new();
    let mut edges = Edges::new();

    let mut id = 0;

    let mut prev_row = Vec::new();

    for line in lines.iter() {
        let mut this_row = Vec::new();

        for (ch, above) in line.chars().zip(prev_row.iter()) {
            let tile = match ch {
                '#' => {
                    this_row.push(None);
                    continue;
                }
                '.' => Tile::Clear,
                c if c.is_ascii_lowercase() => Tile::Key(c),
                c if c.is_ascii_uppercase() => Tile::Door(c),
                '@' => {
                    // TODO record start pos
                    Tile::Start
                }
                _ => panic!()
            };

            nodes.insert(id, tile);

            if !this_row.is_empty() && this_row.last().unwrap().is_some() {
                edges.insert(Edge::new(id, id - 1, 1));
            }

            if let Some(above_id) = above {
                edges.insert(Edge::new(id, *above_id, 1));
            }

            id += 1;
            this_row.push(Some(id));
        }

        prev_row = this_row;
    }

    (nodes, edges)
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

fn simplify_graph(mut nodes: Nodes, edges: Edges) -> (Nodes, Edges) {
    // find edges incident to degree-2 nodes
    let mut incidents: HashMap<NodeID, Option<Edges>> = HashMap::new();

    for edge in edges.iter() {
        use std::collections::hash_map::Entry::*;
        match incidents.entry(edge.min) {
            Occupied(mut entry) => {
                if let Some(set) = entry.get_mut() {
                    if set.len() < 2 {
                        set.insert(*edge);
                    }
                    else {
                        entry.insert(None);
                    }
                }
            }
            Vacant(entry) => {
                let mut new_set = Edges::new();
                new_set.insert(*edge);
                entry.insert(Some(new_set));
            }
        }
    }

    let incidents: HashMap<NodeID, Edges> = incidents.into_iter()
        .filter_map(|(id, edges)| edges.map(move |edges| (id, edges)))
        .collect();

    for edge in incidents.values().flatten() {
    }


    (nodes, edges)
}

fn main() {
    let input = include_str!("../input");
    let lines: Vec<&str> = input.lines().collect();
    let (nodes, edges) = extract_graph(&lines);
    let (nodes, edges) = prune_graph(nodes, edges);
}

