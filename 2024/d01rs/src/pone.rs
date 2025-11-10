use anyhow::Result;
use std::{fs::File, io::Read, iter::zip};

#[derive(Debug)]
pub struct PuzzleInput {
    pub left: Vec<i32>,
    pub right: Vec<i32>,
}

pub fn run() -> Result<()> {
    let mut inp = read_input()?;
    inp.left.sort();
    inp.right.sort();
    let zs: i32 = zip(inp.left, inp.right).map(|(x, y)| (x - y).abs()).sum();
    println!("{:?}", zs);
    Ok(())
}

pub fn read_input() -> Result<PuzzleInput> {
    let mut file = File::open("puzzle_input.txt")?;
    let mut buffer = String::new();
    file.read_to_string(&mut buffer)?;
    let mut _xs = Vec::new();
    let mut _ys = Vec::new();
    for line in buffer.lines() {
        let mut xy = line.split_whitespace();
        let x = xy.next().and_then(to_i32);
        _xs.push(x);
        let y = xy.next().and_then(to_i32);
        _ys.push(y);
    }
    Ok(PuzzleInput {
        left: _xs.into_iter().flatten().collect(),
        right: _ys.into_iter().flatten().collect(),
    })
}

fn to_i32(x: &str) -> Option<i32> {
    match x.parse::<i32>() {
        Ok(i) => Some(i),
        Err(_) => None,
    }
}
