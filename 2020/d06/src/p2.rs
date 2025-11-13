use std::collections::HashMap;

pub fn run(input: &str) {
    let mut sum = 0;
    for groups in input.split("\n\n") {
        let mut group_ans = HashMap::new();
        let group: Vec<_> = groups.split_ascii_whitespace().collect();
        let group_count = group.len();
        for group_member in group {
            for ans in group_member.chars() {
                let c = match group_ans.get(&ans) {
                    Some(i) => i + 1,
                    _ => 1,
                };
                group_ans.insert(ans, c);
            }
        }
        sum += group_ans.into_iter().fold(
            0,
            |acc, (_, count)| if count == group_count { acc + 1 } else { acc },
        )
    }

    println!("{sum}");
}
