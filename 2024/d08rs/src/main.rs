use anyhow::Result;
use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::Read,
};

mod one;
mod two;

pub struct PuzzleInput {
    pub antennas: HashMap<char, HashSet<(usize, usize)>>,
    pub antenna_positions: HashMap<(usize, usize), char>,
    pub max: (usize, usize),
}

fn main() {
    //crate::one::run();
    crate::two::run();
}

pub fn get_input() -> Result<PuzzleInput> {
    let mut antennas: HashMap<char, HashSet<(usize, usize)>> = HashMap::new();
    let mut antenna_positions: HashMap<(usize, usize), char> = HashMap::new();

    let mut file = File::open("input.txt")?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;
    let mut mx = 0;
    let mut my = 0;

    for (y, line) in buf.lines().enumerate() {
        my = y;
        for (x, c) in line.chars().enumerate() {
            mx = x;
            if c == '.' {
                continue;
            }
            match antennas.get(&c) {
                Some(p) => {
                    let mut np = p.clone();
                    np.insert((x, y));
                    antennas.insert(c, np);
                    antenna_positions.insert((x, y), c);
                }
                None => {
                    let mut s = HashSet::new();
                    s.insert((x, y));
                    antennas.insert(c, s);
                    antenna_positions.insert((x, y), c);
                }
            }
        }
    }

    Ok(PuzzleInput {
        antennas,
        antenna_positions,
        max: (mx + 1, my + 1),
    })
}
