use anyhow::Result;

use crate::get_puzzle;

pub fn run() -> Result<i32> {
    let inpt = get_puzzle()?;

    let node_map = inpt.map;
    let paths = inpt.page_updates;

    let mut valid_paths = Vec::new();
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
