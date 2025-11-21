use std::{collections::HashSet, fs::File, io::Read};

fn main() {
    let map = get_input();
    let trail_starts = find_trail_heads(&map);

    for trail_head in trail_starts {
        let tree = Tree::build_tree(trail_head, Tree::new(0), &map);
    }
    
}

pub fn part1() {
    let map = get_input();

    let trail_starts = find_trail_heads(&map);

    let mut sum = 0;
    let mut _sum = 0;
    for trail_head in trail_starts {
        sum += find_nines(trail_head, &map).len();
    }
    println!("{sum}");
}

pub fn get_input() -> Vec<Vec<i32>> {
    let mut file = File::open("test_input").unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();
    let mut map = Vec::new();
    for line in buf.lines() {
        let mut row = Vec::new();
        for c in line.chars() {
            row.push(c.to_digit(10).unwrap().cast_signed());
        }
        map.push(row);
    }

    map
}

fn find_trail_heads(map: &Vec<Vec<i32>>) -> Vec<(usize, usize)> {
    let mut trail_heads = Vec::new();
    for (x, row) in map.iter().enumerate() {
        for (y, c) in row.iter().enumerate() {
            if *c == 0 {
                trail_heads.push((x, y));
            }
        }
    }
    trail_heads
}

fn find_nines(trail_head: (usize, usize), map: &Vec<Vec<i32>>) -> Vec<(usize, usize)> {
    let mut unchecked_paths = Vec::new();
    unchecked_paths.push(trail_head);
    let mut checked_paths = HashSet::new();
    let mut paths = Vec::new();

    while let Some(current_pos) = unchecked_paths.pop() {
        for (x, y) in get_valid_moves(current_pos, map) {
            if checked_paths.contains(&(x, y)) {
                continue;
            }
            if map[x][y] == 9 {
                paths.push((x, y));
            }
            unchecked_paths.push((x, y));
        }

        checked_paths.insert(current_pos);
    }
    paths
}

fn get_valid_moves((x, y): (usize, usize), map: &Vec<Vec<i32>>) -> Vec<(usize, usize)> {
    let lim = map[x][y];
    let mut vec = vec![
        map.get(x)
            .and_then(|ys| ys.get(y + 1))
            .and_then(|i| if i - lim == 1 { Some(()) } else { None })
            .map(|_| (x, y + 1)),
        map.get(x + 1)
            .and_then(|ys| ys.get(y))
            .and_then(|i| if i - lim == 1 { Some(()) } else { None })
            .map(|_| (x + 1, y)),
    ];
    if x >= 1 {
        vec.push(
            map.get(x - 1)
                .and_then(|ys| ys.get(y))
                .and_then(|i| if i - lim == 1 { Some(()) } else { None })
                .map(|_| (x - 1, y)),
        );
    }
    if y >= 1 {
        vec.push(
            map.get(x)
                .and_then(|ys| ys.get(y - 1))
                .and_then(|i| if i - lim == 1 { Some(()) } else { None })
                .map(|_| (x, y - 1)),
        );
    }
    vec.into_iter().filter_map(|s| s).collect()
}

struct Tree {
    val: i32,
    kids: Vec<Tree>,
}

impl Tree {
    pub fn new(val: i32) -> Self {
        Self {
            val,
            kids: Vec::new(),
        }
    }

    fn build_tree(pos: (usize, usize), root: Self, map: &Vec<Vec<i32>>) -> Self {
        let mut kids = Vec::new();

        for (x, y) in get_valid_moves(pos, map) {
            let val = map[x][y];
            if val > root.val {
                kids.push(Self::build_tree(pos, Self::new(val), map));
            }
        }

        Self { kids, ..root }
    }
}
