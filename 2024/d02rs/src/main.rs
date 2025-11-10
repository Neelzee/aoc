use std::{fs::File, io::Read};

use anyhow::Result;

mod one;
mod two;

fn main() -> Result<()> {
    let inpt = get_input()?;
    println!("One: {}", one::run(inpt));
    Ok(())
}

pub struct PuzzleInput {
    reports: Vec<Vec<i32>>,
}

pub fn get_input() -> Result<PuzzleInput> {
    let mut file = File::open("input.txt")?;
    let mut buf = String::new();
    let mut reports = Vec::new();
    file.read_to_string(&mut buf)?;
    for line in buf.lines() {
        reports.push(
            line.split_whitespace()
                .filter_map(|i| i.parse::<i32>().ok())
                .collect(),
        );
    }
    Ok(PuzzleInput { reports })
}

#[test]
fn test_one() {
    let inpt = PuzzleInput {
        reports: vec![
            vec![7, 6, 4, 2, 1], // safe
            vec![1, 2, 7, 8, 9], // unsafe
            vec![9, 7, 6, 2, 1], // unsafe
            vec![1, 3, 2, 4, 5], // unsafe
            vec![8, 6, 4, 4, 1], // unsafe
            vec![1, 3, 6, 7, 9], // safe
        ],
    };
    assert_eq!(one::run(inpt), 2);
}

#[test]
fn test_two() {
    let inpt = PuzzleInput {
        reports: vec![
            //vec![1, 2, 7, 8, 9], // unsafe
            vec![9, 7, 6, 2, 1], // unsafe
                                 //vec![1, 3, 2, 4, 5], // safe
                                 //vec![8, 6, 4, 4, 1], // safe
        ],
    };
    assert_eq!(two::run(inpt), 2);
}
