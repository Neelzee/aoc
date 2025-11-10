use std::collections::{HashMap, HashSet};

use crate::{get_puzzle, Node};
use anyhow::Result;

pub fn run() -> Result<i32> {
    let inpt = get_puzzle()?;
    let mut relations: HashMap<(i32, i32), bool> = HashMap::new();

    let node_map = inpt.map;
    let paths = inpt.page_updates;

    let mut valid_paths = Vec::new();

    for path in paths {
        let mut is_valid = true;
        for i in 1..path.len() {
            let parent = path[i - 1];
            let child = path[i];
            if !node_map.contains_key(&parent) || !node_map.contains_key(&child) {
                is_valid = false;
                break;
            }
            let mut visited = HashSet::new();
            let is_cyclic = _dfs(
                node_map.get(&parent).unwrap().clone(),
                &mut visited,
                &node_map,
            );
            println!("{}", is_cyclic);
            let has_edge = visited.contains(&child);
            if is_cyclic || !has_edge {
                is_valid = false;
                break;
            }
        }
        if is_valid {
            valid_paths.push(path);
        }
    }

    let mut sum = 0;
    for vp in valid_paths {
        sum += vp[vp.len() / 2];
    }

    Ok(sum)
}

pub fn find_relations(parent: i32, child: i32, node_map: &HashMap<i32, Node>) -> bool {
    let parent_node = node_map.get(&parent).unwrap();
    let child_node = node_map.get(&child).unwrap();
    if parent_node.kids_id.contains(&child) {
        return true;
    }
    let mut visited = HashSet::new();
    for n in parent_node
        .kids_id
        .iter()
        .filter_map(|kid| node_map.get(kid))
    {
        if dfs(n.clone(), &mut visited, node_map, child_node) {
            return true;
        }
    }
    false
}

fn dfs(n: Node, visited: &mut HashSet<i32>, nm: &HashMap<i32, Node>, rn: &Node) -> bool {
    if visited.contains(&n.val) {
        return false;
    };
    visited.insert(n.val);
    let mut new = Vec::new();
    for kid in n.kids_id {
        match nm.get(&kid) {
            Some(kn) => {
                if kn.val == rn.val {
                    return true;
                }
                new.push(kn);
            }
            None => {
                return false;
            }
        }
    }
    new.into_iter().any(|n| dfs(n.clone(), visited, nm, rn))
}

fn _dfs(n: Node, visited: &mut HashSet<i32>, nm: &HashMap<i32, Node>) -> bool {
    if visited.contains(&n.val) {
        return true;
    };
    visited.insert(n.val);
    let mut new = Vec::new();
    for kid in n.kids_id {
        match nm.get(&kid) {
            Some(kn) => {
                new.push(kn);
            }
            None => {
                continue;
            }
        }
    }
    for k in new {
        if _dfs(k.clone(), visited, nm) {
            return true;
        }
    }
    false
}
