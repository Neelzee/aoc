use std::{collections::HashMap, str::FromStr};


#[cfg(feature = "p1")]
pub mod p1;
#[cfg(feature = "p2")]
pub mod p2;

#[allow(unused)]
const INPUT: &str = include_str!("../input");

fn main() {
    if cfg!(not(feature = "p1")) && cfg!(not(feature = "p2")) {
        panic!("No feature choosen");
    }

    #[cfg(feature = "p1")]
    p1::run(INPUT);
    #[cfg(feature = "p2")]
    p2::run(INPUT);
}

#[derive(Default, Clone)]
pub struct Map {
    pub tbl: HashMap<(usize, usize), char>,
    pub bot: (usize, usize),
}

impl FromStr for Map {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tbl = HashMap::new();
        let mut bot = (0, 0);
        for (x, row) in s.lines().enumerate() {
            for (y, chr) in row.chars().enumerate() {
                tbl.insert((x, y), chr);
                bot = (x, y);
            }
        }

        Ok(Self { tbl, bot })
    }
}

impl ToString for Map {
    fn to_string(&self) -> String {
        hashmap_to_string(&self.tbl, self.bot.clone())
    }
}

pub fn hashmap_to_string(mp: &HashMap<(usize, usize), char>, bot: (usize, usize)) -> String {
        let mut s = String::new();
        for x in 0..bot.0 {
            let mut w = String::new();
            for y in 0..bot.1 {
                match mp.get(&(x, y)) {
                    Some(c) => w.push(*c),
                    None => unreachable!(),
                }
            }
            w.push('\n');
            s.push_str(&w);
            w.clear();
        }

        s

}