use std::collections::HashSet;

pub fn run(input: &str) {
    let mut sum = 0;
    for group in input.split("\n\n") {
        let mut group_ans = HashSet::new();
        for group_member in group.split_ascii_whitespace() {
            for ans in group_member.chars() {
                group_ans.insert(ans);
            }
        }
        sum += group_ans.len();
    }

    println!("{sum}");
}
