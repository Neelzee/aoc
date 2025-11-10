use std::{fs::File, io::Read};

struct PuzzleInput {
    disk: Vec<isize>,
    count: usize,
}

fn main() {
    let inpt = get_input();
    let mut disk = inpt.disk;
    let mut checked_files: Vec<isize> = Vec::new();
    let mut count = 0;
    while let Some(block) = get_block(&disk, &checked_files) {
        count += 1;
        if count + 10 >= inpt.count {
            println!(
                "Too many iterations:\nDisk:\n{:?}\nChecked files\n{:?}\nCount: {}",
                disk, checked_files, count
            );
        }
        let max_x = block.iter().map(|(i, _)| *i).max().unwrap();
        let len = block.len();
        let id = block[0].1;
        checked_files.push(id);
        match get_free(&disk, len, max_x) {
            Some((s, e)) => {
                for (i, _) in block {
                    disk[i] = -1;
                }
                for i in s..e {
                    disk[i] = id;
                }
            }
            None => continue,
        }
    }
    println!(
        "{}",
        disk.iter()
            .map(|i| i.to_string())
            .collect::<String>()
            .replace("-1", ".")
    );
    println!("Checksum: {}", checksum(&disk));
}

fn checksum(disk: &[isize]) -> usize {
    let mut sum = 0;
    for (i, j) in disk.iter().enumerate() {
        if *j == -1 {
            continue;
        }
        let file_id = *j as usize;
        let index = i;
        sum += index * file_id;
    }
    sum
}

fn get_input() -> PuzzleInput {
    let mut file = File::open("input.txt").unwrap();
    let mut buf = String::new();
    let mut count = 0;
    file.read_to_string(&mut buf).unwrap();
    let mut disk = Vec::new();
    let mut id: isize = -1;
    for (i, c) in buf.trim().chars().enumerate() {
        let size = c.to_string().parse::<isize>().unwrap();
        let cur_id = if i % 2 == 0 {
            id += 1;
            id
        } else {
            -1
        };

        for _ in 0..size {
            disk.push(cur_id);
        }
        count += 1;
    }

    PuzzleInput { disk, count }
}

fn get_free(disk: &[isize], len: usize, max_x: usize) -> Option<(usize, usize)> {
    for i in 0..(max_x + 1) {
        let mut free = true;
        for j in i..(i + len) {
            if disk.len() == j {
                return None;
            }
            if disk[j] != -1 {
                free = false;
                break;
            }
        }
        if free && max_x >= i {
            return Some((i, i + len));
        }
    }
    None
}

fn get_block(disk: &[isize], checked_files: &[isize]) -> Option<Vec<(usize, isize)>> {
    let mut ptr = disk.len() - 1;
    let mut id = disk[ptr];
    while id == -1 || checked_files.contains(&id) {
        ptr -= 1;
        if ptr == 0 {
            return None;
        }
        id = disk[ptr]
    }

    // Gets all the files from ptr to start
    let disk_read = disk[..(ptr + 1)].iter().enumerate();

    let block = disk_read
        .rev()
        .take_while(|(_, j)| **j == id)
        .map(|(i, j)| (i, *j))
        .collect::<Vec<_>>();

    if block.is_empty() {
        None
    } else {
        Some(block)
    }
}
