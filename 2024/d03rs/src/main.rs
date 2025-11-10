use std::{collections::HashMap, fs::File, io::Read};

use regex::{Match, Regex};

use anyhow::Result;

enum PuzzleType {
    Mul,
    Do,
    Dont,
}

fn main() -> Result<()> {
    let re = Regex::new(r#"mul\(\d{1,3},\d{1,3}\)"#)?;
    let do_reg = Regex::new(r#"do\(\)"#).unwrap();
    let dont_reg = Regex::new(r#"don't\(\)"#).unwrap();

    let mut file = File::open("input.txt")?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    let mut sum = 0;

    let mut muls = re
        .captures_iter(&buf)
        .flat_map(|m| m.iter().flatten().collect::<Vec<Match>>())
        .map(|i| (i.start(), i.as_str(), PuzzleType::Mul))
        .collect::<Vec<_>>();

    let mut dos = do_reg
        .captures_iter(&buf)
        .flat_map(|m| m.iter().flatten().collect::<Vec<Match>>())
        .map(|i| (i.start(), i.as_str(), PuzzleType::Do))
        .collect::<Vec<_>>();
    let mut donts = dont_reg
        .captures_iter(&buf)
        .flat_map(|m| m.iter().flatten().collect::<Vec<Match>>())
        .map(|i| (i.start(), i.as_str(), PuzzleType::Dont))
        .collect::<Vec<_>>();

    let mut foo = Vec::new();

    foo.append(&mut muls);
    foo.append(&mut dos);
    foo.append(&mut donts);

    foo.sort_by(|a, b| a.0.cmp(&b.0));

    let mut is_disabled = false;

    for (_, mul, pt) in foo {
        match pt {
            PuzzleType::Mul if !is_disabled => sum += parse_mult(mul),
            PuzzleType::Mul => continue,
            PuzzleType::Do => is_disabled = false,
            PuzzleType::Dont => is_disabled = true,
        }
    }

    println!("{}", sum);

    Ok(())
}

fn parse_mult(mul_str: &str) -> i32 {
    let mut s = mul_str.replace("mul(", "");
    s = s.replace(")", "");
    let mut split = s.split(",");
    let l: i32 = split
        .next()
        .and_then(|l| l.parse().ok())
        .unwrap_or_default();
    let r: i32 = split
        .next()
        .and_then(|r| r.parse().ok())
        .unwrap_or_default();
    l * r
}
