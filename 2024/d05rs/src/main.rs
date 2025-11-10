use anyhow::Result;
use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::Read,
};

mod one;
mod one_one;
mod two;

fn main() -> Result<()> {
    println!("{}", crate::two::run()?);

    Ok(())
}

#[derive(Debug)]
pub struct PuzzleInput {
    pub map: HashMap<i32, Node>,
    pub page_updates: Vec<Vec<i32>>,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Node {
    pub val: i32,
    pub kids_id: Vec<i32>,
}

impl Node {
    pub fn new(val: i32) -> Self {
        Self {
            val,
            kids_id: Vec::new(),
        }
    }

    pub fn add(&mut self, child: &Node) {
        self.kids_id.push(child.val);
    }
}

pub fn get_puzzle() -> Result<PuzzleInput> {
    let mut file = File::open("input.txt")?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    let mut map: HashMap<i32, Node> = HashMap::new();
    let mut page_updates = Vec::new();

    for line in buf.lines() {
        if line.contains("|") {
            let mut split = line.split("|");
            let parent = split.next().and_then(|f| f.parse::<i32>().ok()).unwrap();
            let child = split.next().and_then(|f| f.parse::<i32>().ok()).unwrap();

            // Parent exists, or not, it is the same either way
            let parent_node = get_or_create(parent, &map);
            // Parent does not have child
            if !parent_node.kids_id.contains(&child) {
                let mut new_parent = parent_node.clone();
                // Get or create child, does not add it to the map
                let child_node = get_or_create(child, &map);
                new_parent.add(&child_node);
                // Child is added, so we update map
                map.insert(parent, new_parent);
                // We also add the child
                // Since we either got it from the map, or created it,
                // we don't have to worry about overwriting it
                map.insert(child, child_node);
            }
        } else if line.contains(",") {
            // now we are on the page updates, aka paths in our DAG
            page_updates.push(
                line.split(",")
                    .filter_map(|i| i.parse::<i32>().ok())
                    .collect(),
            );
        }
    }

    Ok(PuzzleInput { map, page_updates })
}

fn get_or_create(node: i32, map: &HashMap<i32, Node>) -> Node {
    map.get(&node).cloned().unwrap_or(Node::new(node))
}
