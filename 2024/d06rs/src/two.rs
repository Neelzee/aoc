use std::collections::HashMap;

use anyhow::Result;

use crate::{get_input, Direction};

pub fn run() -> Result<()> {
    let inpt = get_input()?;
    let dir = inpt.guard_direction;
    let pos = inpt.guard;
    let grid = inpt.map_grid;
    let map = inpt.map;

    let all_pos: Vec<(usize, usize)> = map
        .clone()
        .into_iter()
        .enumerate()
        .flat_map(|(y, r)| {
            r.into_iter()
                .enumerate()
                .map(|(x, _)| (x, y))
                .collect::<Vec<_>>()
        })
        .collect();

    let mut s = 0;

    for (x, y) in all_pos {
        let mut new_map = map.clone();
        let prev = new_map[y][x].clone();
        match prev {
            '.' if (x as i32, y as i32) == pos => continue,
            '#' => continue,
            _ => new_map[y][x] = 'O',
        }
        if check(new_map, dir.clone(), pos.clone(), grid.clone()) {
            s += 1;
        }
    }

    println!("{}", s);

    Ok(())
}

pub fn check(
    map: Vec<Vec<char>>,
    mut guard_direction: Direction,
    mut guard_pos: (i32, i32),
    grid: ((usize, usize), (usize, usize)),
) -> bool {
    let mut new_guard_dir = guard_direction;
    let mut new_map: HashMap<(usize, usize), char> = HashMap::new();
    let mut walked: HashMap<(i32, i32, Direction), usize> = HashMap::new();

    while is_in(guard_pos, grid) {
        if walked
            .get(&(guard_pos.0, guard_pos.1, guard_direction.clone()))
            .is_some_and(|v| *v == 2)
        {
            // we're in a loop
            return true;
        } else {
            let i = walked
                .get(&(guard_pos.0, guard_pos.1, guard_direction.clone()))
                .unwrap_or(&0);
            walked.insert((guard_pos.0, guard_pos.1, guard_direction.clone()), i + 1);
        }

        new_map.insert((guard_pos.0 as usize, guard_pos.1 as usize), 'X');
        match &guard_direction {
            crate::Direction::Up if has_wall_infront(guard_direction, guard_pos, &map) => {
                new_guard_dir = Direction::Right;
            }
            crate::Direction::Up => {
                guard_pos = (guard_pos.0, guard_pos.1 - 1);
            }
            crate::Direction::Down if has_wall_infront(guard_direction, guard_pos, &map) => {
                new_guard_dir = Direction::Left;
            }
            crate::Direction::Down => {
                guard_pos = (guard_pos.0, guard_pos.1 + 1);
            }
            crate::Direction::Left if has_wall_infront(guard_direction, guard_pos, &map) => {
                new_guard_dir = Direction::Up;
            }
            crate::Direction::Left => {
                guard_pos = (guard_pos.0 - 1, guard_pos.1);
            }
            crate::Direction::Right if has_wall_infront(guard_direction, guard_pos, &map) => {
                new_guard_dir = Direction::Down;
            }
            crate::Direction::Right => {
                guard_pos = (guard_pos.0 + 1, guard_pos.1);
            }
        }
        new_map.insert(
            (guard_pos.0 as usize, guard_pos.1 as usize),
            new_guard_dir.to_char(),
        );
        guard_direction = new_guard_dir;
    }

    return false;
}

fn to_map(
    map: &HashMap<(usize, usize), char>,
    old_map: &Vec<Vec<char>>,
    grid: ((usize, usize), (usize, usize)),
) {
    println!("============");
    let ((mix, miy), (max, may)) = grid;
    let mut rows = Vec::new();
    for y in miy..may {
        let mut row = String::new();
        for x in mix..max {
            row.push(*map.get(&(x, y)).unwrap_or(&old_map[y][x]));
        }
        rows.push(row);
    }
    println!("{}", rows.join("\n"));

    println!("============");
}

fn is_in(guard_pos: (i32, i32), grid: ((usize, usize), (usize, usize))) -> bool {
    let (gx, gy) = guard_pos;
    let ((mix, miy), (max, may)) = grid;
    mix as i32 <= gx && gx <= max as i32 && miy as i32 <= gy && gy <= may as i32
}

fn has_wall_infront(dir: Direction, pos: (i32, i32), map: &Vec<Vec<char>>) -> bool {
    let (x, y) = match dir {
        Direction::Up => (pos.0, pos.1 - 1),
        Direction::Down => (pos.0, pos.1 + 1),
        Direction::Left => (pos.0 - 1, pos.1),
        Direction::Right => (pos.0 + 1, pos.1),
    };

    if y >= map.len() as i32 || x >= map[0].len() as i32 || y < 0 || x < 0 {
        return false;
    }

    map[y as usize][x as usize] == '#' || map[y as usize][x as usize] == 'O'
}
