use anyhow::Result;
use std::{collections::HashMap, fs::File, io::Read};

mod one;
mod two;

pub struct PuzzleInput {
    pub rows: Vec<String>,
    pub cols: Vec<String>,
    pub dias: Vec<String>,
}

fn main() -> Result<()> {
    //let sum = crate::one::run()?;
    //println!("Sum: {}", sum);
    println!("{}", crate::two::run()?);
    Ok(())
}

fn get_input() -> Result<PuzzleInput> {
    let mut map = HashMap::new();

    let mut file = File::open("input.txt")?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    let mut rows = Vec::new();
    let mut cols = Vec::new();
    let mut dias: Vec<String> = Vec::new();

    let mut max_x = 0;
    let mut max_y = 0;

    // origin is top left
    for (y, row) in buf.lines().enumerate() {
        rows.push(row.to_string());
        for (x, c) in row.chars().enumerate() {
            map.insert((x, y), c);
            max_x = x;
        }
        max_y = y;
    }

    max_x += 1;
    max_y += 1;

    for x in 0..max_x {
        cols.push((0..max_y).filter_map(|y| map.get(&(x, y))).collect());
    }

    // 0, y
    for x in 0..max_x {
        dias.push(
            (0..max_y)
                .filter_map(|i| {
                    if x < i || max_y < i {
                        return None;
                    }
                    map.get(&(x - i, max_y - i))
                })
                .collect(),
        );
    }

    // x, 0
    for y in 0..max_y {
        dias.push(
            (0..max_x)
                .filter_map(|i| {
                    if y < i || max_x < i {
                        return None;
                    }
                    map.get(&(max_x - i, y - i))
                })
                .collect(),
        );
    }

    // 0, 0 -> x, y
    for x in 0..(max_x * 2) {
        dias.push(
            (0..(max_y * 2))
                .filter_map(|i| {
                    if x < i || (max_y * 2) < i {
                        return None;
                    }
                    map.get(&(x - i, i))
                })
                .collect(),
        );
    }

    dias.push((0..max_x).filter_map(|i| map.get(&(i, i))).collect());

    dias.retain(|s| !s.is_empty());

    Ok(PuzzleInput { rows, cols, dias })
}
