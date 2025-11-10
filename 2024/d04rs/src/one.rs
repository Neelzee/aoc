use anyhow::Result;
use fancy_regex::Regex;
use once_cell::sync::Lazy;

use crate::get_input;

pub fn run() -> Result<i32> {
    let inpt = get_input()?;

    let rows: i32 = inpt.rows.iter().map(|l| check(l)).sum();
    let cols: i32 = inpt.cols.iter().map(|l| check(l)).sum();
    let dias: i32 = inpt.dias.iter().map(|l| check(l)).sum();

    Ok(rows + cols + dias)
}

static REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r#"(?=(XMAS|SAMX))"#).unwrap());

fn check(line: &str) -> i32 {
    REGEX
        .captures_iter(line)
        .flatten()
        .map(|i| i.len() - 1)
        .sum::<usize>() as i32
}
