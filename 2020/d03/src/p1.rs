pub fn run(input: &str) {
    let map: Vec<Vec<char>> = input.lines().map(|c| c.chars().collect()).collect();

    let width = map.get(0).unwrap().len();
    let heigth = map.len();

    let mut pos = (0, 0);

    let mut trees = 0;

    while heigth > pos.0 {
        match map.get(pos.0).and_then(|row| row.get(pos.1 % width)) {
            None => unreachable!(),
            Some('#') => trees += 1,
            _ => (),
        }

        pos = (pos.0 + 1, pos.1 + 3);
    }

    println!("{:?}", trees);
}
