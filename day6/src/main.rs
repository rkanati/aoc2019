
use {
    std::{
        collections::hash_map::HashMap,
        io::{BufRead, BufReader},
        fs::File,
    },
    multimap::MultiMap,
};

type Node = String;
type NodeRef<'a> = &'a str;
type Tree<'a>    = MultiMap<Node, Node>;
type InvTree<'a> = HashMap<Node, Node>;

fn descend<'a>(tree: &Tree<'a>, node: NodeRef<'a>, depth: u32) -> u32 {
    depth + if let Some(children) = tree.get_vec(node) {
        children.iter().map(|child| descend(tree, child, depth+1)).sum::<u32>()
    }
    else {
        0
    }
}

fn lineage<'a>(invtree: &'a InvTree<'a>, mut node: NodeRef<'a>) -> Vec<Node> {
    let mut line = Vec::new();
    while let Some(parent) = invtree.get(node) {
        line.push(parent.clone());
        node = &parent;
    }
    line
}

fn nearest_common<'a>(
    invtree: &InvTree<'a>,
    a: NodeRef<'a>,
    b: NodeRef<'a>)
    -> (Node, usize, usize)
{
    let line_a = lineage(&invtree, a);
    let line_b = lineage(&invtree, b);

    for (dist_a, ancestor_a) in line_a.iter().enumerate() {
        for (dist_b, ancestor_b) in line_b.iter().enumerate() {
            if ancestor_a == ancestor_b {
                return (ancestor_a.clone(), dist_a, dist_b);
            }
        }
    }

    panic!()
}

fn invert_tree<'a>(tree: &Tree<'a>) -> InvTree<'a> {
    let mut invtree = InvTree::new();
    for (parent, children) in tree.iter_all() {
        for child in children.iter() {
            invtree.insert(child.clone(), parent.clone());
        }
    }
    invtree
}

fn main() {
    let file = File::open("input").expect("opening input");
    let input = BufReader::new(file);
    let tree: Tree = input
        .lines()
        .map(|line| {
            let line = line.expect("i/o");
            let mut parts = line.split(')')
                .map(|s| s.to_string());
            let parent = parts.next().unwrap();
            let child  = parts.next().unwrap();
            (parent, child)
        })
        .collect();

    let total_orbits = descend(&tree, "COM", 0);
    println!("Part 1: {} total orbits", total_orbits);

    let invtree = invert_tree(&tree);
    let (via, dist_a, dist_b) = nearest_common(&invtree, "YOU", "SAN");
    println!("Part 2: {} transfers (via {})", dist_a + dist_b, via);
}

