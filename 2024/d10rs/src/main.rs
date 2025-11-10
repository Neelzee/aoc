use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::Read,
};

fn main() {
    println!("Hello, world!");
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Node {
    pub id: usize,
    pub val: usize,
    pub loc: (usize, usize),
    pub kids_id: Vec<usize>,
}

pub struct PuzzleInput {
    pub nodes: Vec<Node>,
}

pub fn get_input() -> PuzzleInput {
    let mut file = File::open("input.txt").unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();
    let mut nodes = Vec::new();
    let mut nodes_map = HashMap::new();
    let mut id = 0;
    for (y, line) in buf.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '.' {
                continue;
            }
            let node = Node {
                id,
                val: c.to_string().parse::<usize>().unwrap(),
                loc: (x, y),
                kids_id: Vec::new(),
            };
            nodes.push(node.clone());
            nodes_map.insert((x, y), node);
            id += 1;
        }
    }

    for node in nodes {
        let mut unchecked = get_around(node.loc);
        while let Some(pos) = unchecked.pop() {
            todo!()
        }
    }

    PuzzleInput { nodes: Vec::new() }
}

pub fn get_around((x, y): (usize, usize)) -> Vec<(usize, usize)> {
    let mut pos = vec![(x + 1, y + 1)];
    if x != 1 {
        pos.push((x - 1, y));
    }
    if y != 1 {
        pos.push((x, y - 1));
    }
    if x != 1 && y != 1 {
        pos.push((x - 1, y - 1));
    }
    pos
}
