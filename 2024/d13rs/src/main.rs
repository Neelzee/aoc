//! Claw Contraption
//!
//! https://adventofcode.com/2024/day/13
//!
use std::{fs::File, io::Read};

#[derive(Debug)]
pub struct Machine {
    pub a: (isize, isize),
    pub b: (isize, isize),
    pub goal: (isize, isize),
}

fn main() {
    println!("{:?}", get_input());
}

pub fn get_input() -> Vec<Machine> {
    let mut machines = Vec::new();
    let mut file = File::open("input.txt").unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();
    let mut lines = buf.lines();
    loop {
        let raw_a = lines.next().unwrap();
        let raw_b = lines.next().unwrap();
        let raw_goal = lines.next().unwrap();

        machines.push(Machine {
            a: btn(raw_a),
            b: btn(raw_b),
            goal: prize(raw_goal),
        });

        if lines.next().is_none() {
            break;
        }
    }
    machines
}

fn btn(s: &str) -> (isize, isize) {
    let binding = s
        .replace("Button", "")
        .replace(":", "")
        .replace("A", "")
        .replace("B", "")
        .replace("X", "")
        .replace("Y", "")
        .replace("+", "");
    let mut s = binding.trim().split(",");

    (
        s.next().unwrap().trim().parse::<isize>().unwrap(),
        s.next().unwrap().trim().parse::<isize>().unwrap(),
    )
}

fn prize(s: &str) -> (isize, isize) {
    let binding = s
        .replace("Prize:", "")
        .replace("X", "")
        .replace("Y", "")
        .replace("=", "");
    let mut s = binding.trim().split(",");

    (
        s.next().unwrap().trim().parse::<isize>().unwrap(),
        s.next().unwrap().trim().parse::<isize>().unwrap(),
    )
}
