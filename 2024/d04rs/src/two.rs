use anyhow::Result;
use std::{collections::HashMap, fs::File, io::Read};

pub fn run() -> Result<i32> {
    let mut map = HashMap::new();

    let mut file = File::open("input.txt")?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    let mut max_x = 0;
    let mut max_y = 0;

    // origin is top left
    for (y, row) in buf.lines().enumerate() {
        for (x, c) in row.chars().enumerate() {
            map.insert((x, y), c);
            max_x = x;
        }
        max_y = y;
    }

    max_x += 1;
    max_y += 1;

    let mut sum = 0;

    for x in 1..max_x {
        for y in 1..max_y {
            sum += match map.get(&(x, y)) {
                Some('A') => {
                    let dia_one = map.get(&(x - 1, y - 1)).is_some_and(|c| c == &'M')
                        && map.get(&(x + 1, y + 1)).is_some_and(|c| c == &'S')
                        || map.get(&(x - 1, y - 1)).is_some_and(|c| c == &'S')
                            && map.get(&(x + 1, y + 1)).is_some_and(|c| c == &'M');

                    let dia_two = map.get(&(x + 1, y - 1)).is_some_and(|c| c == &'M')
                        && map.get(&(x - 1, y + 1)).is_some_and(|c| c == &'S')
                        || map.get(&(x + 1, y - 1)).is_some_and(|c| c == &'S')
                            && map.get(&(x - 1, y + 1)).is_some_and(|c| c == &'M');

                    if dia_one && dia_two {
                        1
                    } else {
                        0
                    }
                }
                _ => 0,
            }
        }
    }

    Ok(sum)
}
