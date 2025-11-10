use anyhow::Result;
use std::{fs::File, io::Read};

mod one;
mod two;

pub struct PuzzleInput {
    pub map: Vec<Vec<char>>,
    pub guard: (i32, i32),
    pub guard_direction: Direction,
    pub map_grid: ((usize, usize), (usize, usize)),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    pub fn to_char(&self) -> char {
        match self {
            Direction::Up => '^',
            Direction::Down => 'v',
            Direction::Left => '<',
            Direction::Right => '>',
        }
    }
}

fn main() -> Result<()> {
    crate::two::run()?;
    Ok(())
}

pub fn get_input() -> Result<PuzzleInput> {
    let mut map = Vec::new();

    let mut file = File::open("input.txt")?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;
    let mut guard = (0, 0);
    let mut guard_direction = Direction::Up;
    let mut mx = 0;
    let mut my = 0;

    for (y, line) in buf.lines().enumerate() {
        let mut row = Vec::new();
        for (x, c) in line.chars().enumerate() {
            row.push(c);
            match c {
                '<' => {
                    guard_direction = Direction::Left;
                    guard = (x as i32, y as i32);
                }
                '^' => {
                    guard_direction = Direction::Up;
                    guard = (x as i32, y as i32);
                }
                '>' => {
                    guard_direction = Direction::Right;
                    guard = (x as i32, y as i32);
                }
                'v' => {
                    guard_direction = Direction::Down;
                    guard = (x as i32, y as i32);
                }
                _ => (),
            }
            mx = x;
        }
        map.push(row);
        my = y;
    }

    Ok(PuzzleInput {
        map,
        guard,
        guard_direction,
        map_grid: ((0, 0), (mx + 1, my + 1)),
    })
}
