pub fn run(input: &str) {
    let result = input
        .split("\n\n")
        .map(|s| {
            s.split_ascii_whitespace().map(|y| {
                let mut kvp = y.split(":");
                let key = kvp.next().unwrap();
                let val = kvp.next().unwrap();
                (key.to_string(), val.to_string())
            })
        })
        .map(|xs| {
            let ys = xs.collect::<Vec<_>>();
            ys.iter().any(|(x, _)| x == "byr")
                && ys.iter().any(|(x, _)| x == "iyr")
                && ys.iter().any(|(x, _)| x == "eyr")
                && ys.iter().any(|(x, _)| x == "hgt")
                && ys.iter().any(|(x, _)| x == "hcl")
                && ys.iter().any(|(x, _)| x == "ecl")
                && ys.iter().any(|(x, _)| x == "pid")
        })
        .filter(|x| *x)
        .count();

    println!("{result}");
}
