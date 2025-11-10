use std::str::FromStr;

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

pub struct PasswordRule {
    pub min: usize,
    pub max: usize,
    pub chr: char,
    pub pswrd: String
}

impl FromStr for PasswordRule {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pswrd = s.split(": ").last().ok_or("Invalid input")?.to_string();
        let a = s.split(":").next().ok_or("Invalid rules")?;

        let chr = a.chars().last().ok_or("Invalid char")?;

        let (min, max): (usize, usize) = a.split(" ").next().and_then(|c| {
            let mut range = c.split("-");
            Some((range.next()?.parse::<usize>().ok()?, range.next()?.parse::<usize>().ok()?))
        }).ok_or("Invalid range")?;

        Ok(PasswordRule { min, max, chr, pswrd })
    }
}