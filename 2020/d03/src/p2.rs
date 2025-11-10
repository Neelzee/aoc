pub fn run(input: &str) {
    let map: Vec<Vec<char>> = input.lines().map(|c| c.chars().collect()).collect();
    let slopes = [
        (1, 1),
        (1, 3),
        (1, 5),
        (1, 7),
        (2, 1),
    ];

    let result = slopes.into_iter().fold(1,|acc, pos| check_slope(pos, &map) * acc);

    println!("{result}");
}

fn check_slope((x, y): (usize, usize), xs: &Vec<Vec<char>>) -> usize {
    let width = xs.get(0).unwrap().len();
    let heigth = xs.len();

    let mut pos = (0, 0);

    let mut trees = 0;

    while heigth > pos.0 {
        match xs.get(pos.0).and_then(|row| row.get(pos.1 % width)) {
            None => unreachable!(),
            Some('#') => trees += 1,
            _ => (),
        }

        pos = (pos.0 + x, pos.1 + y);
    }

    trees
}