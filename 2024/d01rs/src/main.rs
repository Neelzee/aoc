use anyhow::Result;
use pone::read_input;
use std::collections::HashMap;

mod pone;

fn main() -> Result<()> {
    let inp = read_input()?;

    let mut appearance_map: HashMap<i32, i32> = HashMap::new();

    for y in inp.right {
        if let Some(v) = appearance_map.get(&y) {
            appearance_map.insert(y, v + 1);
        } else {
            appearance_map.insert(y, 1);
        }
    }

    let mut similarity_score = 0;

    for x in inp.left {
        let appearance = appearance_map.get(&x).unwrap_or(&0);
        similarity_score += x * appearance;
    }

    println!("{}", similarity_score);

    Ok(())
}
