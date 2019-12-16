
use {
    intcode_rk::{VM, VMResult, parse_programs},
    std::{
        collections::hash_map::{self, HashMap},
        cmp::Reverse,
    },
    priority_queue::PriorityQueue,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct V2 {
    pub x: i32,
    pub y: i32
}

impl V2 {
    fn new(x: i32, y: i32) -> V2 {
        V2 { x, y }
    }
}

impl std::ops::Add for V2 {
    type Output = V2;
    fn add(self, rhs: V2) -> V2 {
        V2 { x: self.x + rhs.x, y: self.y + rhs.y }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Direction {
    North,
    South,
    West,
    East,
}

impl Direction {
    fn each() -> impl Iterator<Item = Direction> {
        use Direction::*;
        static DIRS: [Direction; 4] = [ North, South, West, East ];
        DIRS.iter().copied()
    }

    fn opposite(self) -> Direction {
        use Direction::*;
        match self {
            North  => South,
            South  => North,
            West   => East,
            East   => West
        }
    }
}

impl From<Direction> for V2 {
    fn from(dir: Direction) -> V2 {
        use Direction::*;
        match dir {
            North  => V2::new( 0,  1),
            South  => V2::new( 0, -1),
            West   => V2::new(-1,  0),
            East   => V2::new( 1,  0)
        }
    }
}

impl From<Direction> for i64 {
    fn from(dir: Direction) -> i64 {
        use Direction::*;
        match dir {
            North  => 1,
            South  => 2,
            West   => 3,
            East   => 4
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Tile {
    Clear,
    Wall,
    Goal
}

impl From<Tile> for char {
    fn from(tile: Tile) -> char {
        match tile {
            Tile::Clear => ' ',
            Tile::Wall  => '▒',
            Tile::Goal  => '⚑'
        }
    }
}

impl From<i64> for Tile {
    fn from(i: i64) -> Tile {
        use Tile::*;
        match i {
            0 => Wall,
            1 => Clear,
            2 => Goal,
            _ => panic!("bad tile")
        }
    }
}

fn move_bot(vm: &mut VM, direction: Direction) -> Tile {
    vm.feed_input(direction.into());
    let tile: Tile = vm.run().expect_output("expected bot status report")
        .into();
    vm.run().expect_wait_input("");
    tile
}

fn traverse_recursive(
    vm: &mut VM,
    adjs: &mut HashMap<V2, Vec<V2>>,
    goal: &mut Option<V2>,
    position: V2)
{
    for direction in Direction::each() {
        let step: V2 = direction.into();
        let new_position = position + step;

        use hash_map::Entry::*;
        match adjs.entry(new_position) {
            Occupied(mut entry) => {
                entry.get_mut().push(position);
                adjs.entry(position).or_insert(Vec::new()).push(new_position);
            }
            Vacant(entry) => {
                let tile = move_bot(vm, direction);

                if tile == Tile::Goal {
                    *goal = Some(new_position);
                }

                // Move and recurse
                if tile != Tile::Wall {
                    entry.insert(vec![position]);
                    adjs.entry(position).or_insert(Vec::new()).push(new_position);
                    traverse_recursive(vm, adjs, goal, new_position);
                    move_bot(vm, direction.opposite());
                }
            }
        }
    }

}

fn traverse(vm: &mut VM, start: V2) -> (HashMap<V2, Vec<V2>>, V2) {
    let mut adjs: HashMap<V2, Vec<V2>> = HashMap::new();
    let mut goal = None;
    traverse_recursive(vm, &mut adjs, &mut goal, start);
    (adjs, goal.unwrap())
}

fn dijkstra(adjs: &HashMap<V2, Vec<V2>>, start: V2)
    -> (HashMap<V2, i32>, HashMap<V2, V2>)
{
    let nodes = {
        let mut nodes: Vec<V2> = adjs.keys().copied().collect();
        nodes.sort_unstable();
        nodes
    };

    let mut dists: HashMap<V2, Option<i32>> = nodes.iter()
        .map(|pos| {
            let dist = if *pos == start { Some(0) } else { None };
            (*pos, dist)
        })
        .collect();

    let mut predecessors: HashMap<V2, V2> = HashMap::new();

    let mut queue: PriorityQueue<V2, Reverse<i32>> = dists.iter()
        .map(|(pos, dist)| (*pos, Reverse(dist.unwrap_or(std::i32::MAX))))
        .collect();

    while let Some((u, _)) = queue.pop() {
        let neighbors = if let Some(neighbors) = adjs.get(&u) {
            neighbors
        }
        else {
            continue;
        };

        for v in neighbors.iter() {
            let new_dist = dists[&u].unwrap_or(0) + 1;
            if new_dist < dists[&v].unwrap_or(std::i32::MAX) {
                dists.insert(*v, Some(new_dist));
                predecessors.insert(*v, u);
                queue.change_priority(&v, Reverse(new_dist));
            }
        }
    }

    let dists = dists.iter()
        .map(|(pos, dist)| (*pos, dist.unwrap_or(std::i32::MAX)))
        .collect();

    (dists, predecessors)
}

fn main() {
    let input = include_str!("../input");
    let mem = parse_programs(input).nth(0).expect("loading program");

    let mut vm = VM::new(mem);

    loop {
        match vm.run() {
            VMResult::WaitInput => { break }
            _                   => { }
        }
    }

    let (adjs, goal_pos) = traverse(&mut vm, V2::new(0, 0));

    let mut min_x = 0;
    let mut max_x = 0;
    let mut min_y = 0;
    let mut max_y = 0;
    for pos in adjs.keys() {
        min_x = min_x.min(pos.x);
        min_y = min_y.min(pos.y);
        max_x = max_x.max(pos.x);
        max_y = max_y.max(pos.y);
    }

    // render
    let width  = (max_x - min_x + 1) as usize;
    let height = (max_y - min_y + 1) as usize;
    let mut image: Vec<_> = std::iter::repeat(char::from(Tile::Wall))
        .take(width*height)
        .collect();
    for pos in adjs.keys() {
        let ix = (pos.x - min_x) as usize;
        let iy = (pos.y - min_y) as usize;
        image[iy * width + ix] = match *pos {
            p if p == V2::new(0, 0) => 'S',
            p if p == goal_pos      => char::from(Tile::Goal),
            _                       => char::from(Tile::Clear),
        };
    }

    let (dists, preds) = dijkstra(&adjs, goal_pos);

    {   let mut pos = V2::new(0, 0);
        loop {
            let ix = (pos.x - min_x) as usize;
            let iy = (pos.y - min_y) as usize;
            let cell = &mut image[iy * width + ix];
            if *cell == char::from(Tile::Clear) { *cell = '·'; }
            pos = if let Some(pred) = preds.get(&pos) { *pred }
                  else { break; }
        }
    }

    let wall_char = char::from(Tile::Wall);
    let border: String = std::iter::repeat(wall_char).take(width + 2).collect();
    println!("{}", border);
    for row in image.chunks_exact(width) {
        let string: String = row.iter().collect();
        println!("{}{}{}", wall_char, string, wall_char);
    }
    println!("{}", border);

    let goal_dist = dists[&V2::new(0, 0)];
    println!("Part 1: goal dist: {}", goal_dist);

    let max_dist = dists.values().max().unwrap();
    println!("Part 2: fill time: {} min", max_dist);
}

