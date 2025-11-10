use crate::PasswordRule;

pub fn run(input: &str) {
    let count = input.lines().flat_map(|s| s.parse::<PasswordRule>()).filter(|p| p.is_valid()).count();
    println!("{count}");
}

impl PasswordRule {
    pub fn is_valid_two(&self) -> bool {
        let pswrd = &self.pswrd;
        let chrs = pswrd.chars().collect::<Vec<_>>();

        let fst_char = chrs.get(self.min - 1).is_some_and(|c| c == &self.chr);
        let snd_char = chrs.get(self.max - 1).is_some_and(|c| c == &self.chr);

        fst_char ^ snd_char
    }
}