use regex::Regex;
use std::sync::LazyLock;

pub fn run(input: &str) {
    let result = input
        .split("\n\n")
        .map(|s| {
            s.split_ascii_whitespace()
                .map(|y| {
                    let mut kvp = y.split(":");
                    let key = kvp.next().unwrap();
                    let val = kvp.next().unwrap();
                    (key, val)
                })
                .collect::<Vec<_>>()
        })
        .filter(|passport| {
            passport.iter().any(|(x, val)| *x == "byr" && byr(val))
                && passport.iter().any(|(x, val)| *x == "iyr" && iyr(val))
                && passport.iter().any(|(x, val)| *x == "eyr" && eyr(val))
                && passport.iter().any(|(x, val)| *x == "hgt" && hgt(val))
                && passport.iter().any(|(x, val)| *x == "hcl" && hcl(val))
                && passport.iter().any(|(x, val)| *x == "ecl" && ecl(val))
                && passport.iter().any(|(x, val)| *x == "pid" && pid(val))
        })
        .count();

    println!("{result}");
}

const COLOR_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new("#[0-9a-f]{6}").unwrap());
const PID_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new("^[0-9]{9}$").unwrap());

fn byr(val: &str) -> bool {
    val.parse::<usize>().is_ok_and(|x| 1920 <= x && x <= 2002)
}

fn iyr(val: &str) -> bool {
    val.parse::<usize>().is_ok_and(|x| 2010 <= x && x <= 2020)
}

fn eyr(val: &str) -> bool {
    val.parse::<usize>().is_ok_and(|x| 2020 <= x && x <= 2030)
}

fn hgt(val: &str) -> bool {
    if val.ends_with("in") {
        val.replace("in", "")
            .trim()
            .parse::<usize>()
            .is_ok_and(|x| 59 <= x && x <= 76)
    } else if val.ends_with("cm") {
        val.replace("cm", "")
            .trim()
            .parse::<usize>()
            .is_ok_and(|x| 150 <= x && x <= 193)
    } else {
        false
    }
}

fn hcl(val: &str) -> bool {
    COLOR_REGEX.is_match(val)
}

fn ecl(val: &str) -> bool {
    matches!(val, "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth")
}

fn pid(val: &str) -> bool {
    PID_REGEX.is_match(val)
}
