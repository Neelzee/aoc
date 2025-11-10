use std::str::FromStr;

const INPUT: &str = include_str!("../day2");

fn main() {
    one();
}

fn one() {
    let count = INPUT.lines().flat_map(|s| s.parse::<PasswordRule>()).filter(|p| p.is_valid()).count();
    println!("{count}");
    let count_two = INPUT.lines().flat_map(|s| s.parse::<PasswordRule>()).filter(|p| p.is_valid_two()).count();
    println!("{count_two}");
}


struct PasswordRule {
    min: usize,
    max: usize,
    chr: char,
    pswrd: String
}

impl PasswordRule {
    pub fn is_valid(&self) -> bool {
        let pswrd = &self.pswrd;
        let chr_count = pswrd.chars().filter(|c| c == &self.chr).count();
        self.min <= chr_count && chr_count <= self.max
    }

    pub fn is_valid_two(&self) -> bool {
        let pswrd = &self.pswrd;
        let chrs = pswrd.chars().collect::<Vec<_>>();

        let fst_char = chrs.get(self.min - 1).is_some_and(|c| c == &self.chr);
        let snd_char = chrs.get(self.max - 1).is_some_and(|c| c == &self.chr);

        fst_char ^ snd_char
    }
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