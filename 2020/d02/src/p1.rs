use crate::PasswordRule;

pub fn run(input: &str) {
    let count = input.lines().flat_map(|s| s.parse::<PasswordRule>()).filter(|p| p.is_valid()).count();
    println!("{count}");
}


impl PasswordRule {
    pub fn is_valid(&self) -> bool {
        let pswrd = &self.pswrd;
        let chr_count = pswrd.chars().filter(|c| c == &self.chr).count();
        self.min <= chr_count && chr_count <= self.max
    }
}
