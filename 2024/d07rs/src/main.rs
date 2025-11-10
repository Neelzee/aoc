use anyhow::Result;
use itertools::{repeat_n, Itertools};
use std::{fs::File, io::Read};

#[derive(Debug)]
pub struct PuzzleInput {
    pub equations: Vec<(isize, Vec<isize>)>,
}

fn main() -> Result<()> {
    let s = run()?;
    println!("{}", s);
    Ok(())
}

fn get_input() -> Result<PuzzleInput> {
    let mut equations = Vec::new();

    let mut file = File::open("input.txt")?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    for line in buf.lines() {
        let mut inp = line.split(":");
        let s = inp.next().unwrap().trim().parse::<isize>()?;
        let nums = inp
            .collect::<String>()
            .split_whitespace()
            .filter_map(|n| n.trim().parse::<isize>().ok())
            .collect();
        equations.push((s, nums))
    }

    Ok(PuzzleInput { equations })
}

pub fn run() -> Result<isize> {
    let inpt = get_input()?;

    let mut sum = 0;
    for eq in inpt.equations {
        let og_sum = eq.0.clone();
        if get_all_ops(eq).into_iter().any(|op| eval(op) == og_sum) {
            sum += og_sum;
        }
    }

    Ok(sum)
}

enum Op {
    Add(Box<Op>, Box<Op>),
    Mul(Box<Op>, Box<Op>),
    Con(Box<Op>, Box<Op>),
    Val(isize),
}

fn eval(op: Op) -> isize {
    match op {
        Op::Add(l, r) => eval(*l) + eval(*r),
        Op::Mul(l, r) => eval(*l) * eval(*r),
        Op::Val(i) => i,
        Op::Con(l, r) => {
            let left = eval(*l);
            let right = eval(*r);
            let mut ls = left.to_string();
            let rs = right.to_string();
            ls.push_str(&rs);
            return ls.parse::<isize>().unwrap();
        }
    }
}

fn get_all_ops(equation: (isize, Vec<isize>)) -> Vec<Op> {
    let (_s, numbers) = equation;
    let mut operations = Vec::new();
    for ops in repeat_n(vec!['+', '*', '|'].iter(), numbers.len())
        .multi_cartesian_product()
        .unique()
    {
        operations.push(generate_op(numbers.clone(), ops));
    }

    operations
}

fn generate_op(mut numbers: Vec<isize>, mut ops: Vec<&char>) -> Op {
    let mut prev = Op::Val(numbers.remove(0));
    ops.reverse();
    numbers.reverse();

    while let Some(op) = ops.pop() {
        if let Some(n) = numbers.pop() {
            match op {
                '*' => prev = Op::Mul(Box::new(prev), Box::new(Op::Val(n))),
                '+' => prev = Op::Add(Box::new(prev), Box::new(Op::Val(n))),
                '|' => prev = Op::Con(Box::new(prev), Box::new(Op::Val(n))),
                _ => unreachable!(),
            }
        } else {
            break;
        }
    }

    prev
}
