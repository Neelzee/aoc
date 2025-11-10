use crate::{get_puzzle, Node};
use anyhow::Result;

pub fn run() -> Result<i32> {
    let inpt = get_puzzle()?;

    let node_map = inpt.map;
    let paths = inpt.page_updates;

    let mut not_valid_paths = Vec::new();
    for path in paths {
        let mut is_valid = true;
        let mut htap = path.clone();
        htap.reverse();
        for (i, p) in htap.iter().enumerate() {
            for k in (i + 1)..htap.len() {
                match node_map.get(p) {
                    Some(p_node) => {
                        if p_node.kids_id.contains(&htap[k]) {
                            is_valid = false;
                            break;
                        }
                    }
                    None => {
                        is_valid = false;
                        break;
                    }
                }
            }
            if !is_valid {
                break;
            }
        }
        if !is_valid {
            not_valid_paths.push(path);
        }
    }

    let mut super_valid_paths = Vec::new();
    for path in not_valid_paths {
        let mut new_path: Vec<i32> = Vec::new();
        let mut unchecked_nodes = path.clone();
        while !unchecked_nodes.is_empty() {
            let c = unchecked_nodes.clone();
            'pointer_loop: for (i, id) in c.iter().enumerate() {
                let pointer = node_map.get(&id).unwrap();
                for j in i..unchecked_nodes.len() {
                    let pointee = node_map.get(&unchecked_nodes[j]).unwrap();
                    if pointer.kids_id.contains(&pointee.val) {
                        continue 'pointer_loop;
                    }
                }
                unchecked_nodes.remove(i);
                new_path.push(*id);
                break;
            }
        }
        super_valid_paths.push(new_path);
    }

    let mut sum = 0;
    for vp in super_valid_paths {
        sum += vp[vp.len() / 2];
    }

    Ok(sum)
}
