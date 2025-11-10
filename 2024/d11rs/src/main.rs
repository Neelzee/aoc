//! [Plutonian Pebbles](https://adventofcode.com/2024/day/11)
//! 
//! To solve this task in a reasonable time, memoization is needed.
//! My idea was, to get the nth iteration of some input stone _A_, call the function f(A, n),
//! which does a lookup on some cache, finding A after n - 1 iterations, or calculating it.
//!
//! The reason I wanted to this, is because I noticed this:
//! Given the input: 0, 1, 10, 99 and 990, we will map the function f, which gets the next
//! iteration of a stone.
//! ```
//! First iteration   Second iteration   Third iteration   Fourth iteration   Fifth iteration
//! |                 |                  |                 |                  |
//! 0              -> 1               -> 2024           -> 20, 24          -> 2, 0, 2, 4
//! ```
//!
//! And continuing for the next stone in the input:
//! ```
//! First iteration    Second iteration Third iteration   Fourth iteration   Fifth iteration
//! |                  |                |                 |                  |
//! 1               -> 2024           -> 20, 24        -> 2, 0, 2, 4      -> 4048, 1, 4048, 8096
//! ```
//!
//! Note, for stone 1 the four first iterations, we have the same path that stone 0 had. So instead
//! of doing those calculation, we should do something like:
//! ```
//! First iteration    Fifth iteration
//! |                  |
//! 1               -> 4048, 1, 4048, 8096
//! ```
//!
//! But I am struggling to get this to work, mostly because I have not looked at how it supposed to
//! be, and just tried to implement it myself. ¯\_(ツ)_/¯
//!
//! Given this haskell syntax, where digits :: Int -> [Int], aka. returns all digits in a given
//! number
//! ```haskell
//! f :: Int -> Int -> [Int]
//! f 0 n = [n]
//! f n 0 = f (n - 1) [1]
//! f n s
//!   | even len = concatMap f (n - 1) [beg, end]
//!   | otherwise = [2024 * n]
//!   where
//!    digs = digits n
//!    len = length digs
//!    beg = take (len `div` 2) digs
//!    end = drop (len `div` 2) digs
//! ```
//!
//! Is something I want in Rust, except that for each step, I do a lookup in a cache, to see if I
//! have calculated f n s before, if it's a cache hit, then just return this, if it's a half-hit,
//! i.e. I have calculated f (n - a) s, then I can just start from f (???) s.

use std::{collections::HashMap, fs::File, io::Read};

fn main() {
    let stones = get_input();
    let mut cache: HashMap<usize, Vec<(usize, Vec<usize>)>> = HashMap::new();
    let mut final_stones: Vec<usize> = Vec::new();

    for stone in stones {
        final_stones.append(&mut iterate(stone, 75, &mut cache));
    }

    println!("{}", final_stones.len());
}

fn iterate(
    stone: usize,
    current_index: usize,
    cache: &mut HashMap<usize, Vec<(usize, Vec<usize>)>>,
) -> Vec<usize> {
    match cache.get(&stone) {
        Some(v) => {
            // Filter results based on the current index
            let xs: Vec<_> = v
                .clone()
                .into_iter()
                .filter(|(x, _)| current_index >= *x)
                .collect();

            // Find the closest index (sort by abs difference)
            if let Some((closest_index, closest_list)) =
                xs.iter().min_by_key(|(x, _)| (current_index.abs_diff(*x)))
            {
                if *closest_index == current_index {
                    return closest_list.clone();
                }
                // Recursive call with closest match
                closest_list
                    .iter()
                    .flat_map(|x| iterate(*x, current_index - 1, cache))
                    .collect()
            } else {
                // If no closest found, fallback to a new calculation
                let intermid_stones = sub_iterate(stone);
                cache.insert(stone, vec![(current_index, intermid_stones.clone())]);
                intermid_stones
            }
        }
        None if current_index == 0 => {
            // If no cache and we're at the starting point, apply sub_iterate
            let stones = sub_iterate(stone);
            cache.insert(stone, vec![(current_index, stones.clone())]);
            stones
        }
        None => {
            // If no cache and we're not at the starting point, recalculate
            let intermid_stones = sub_iterate(stone);
            cache.insert(stone, vec![(1, intermid_stones.clone())]);
            intermid_stones
                .iter()
                .flat_map(|x| iterate(*x, current_index - 1, cache))
                .collect()
        }
    }
}

fn sub_iterate(stone: usize) -> Vec<usize> {
    match stone {
        0 => vec![1],
        n => {
            let digs = digitize(n);
            let len = digs.len();
            if len % 2 == 0 {
                vec![
                    undigitize(digs.iter().cloned().take(len / 2).collect()),
                    undigitize(digs.into_iter().skip(len / 2).collect()),
                ]
            } else {
                vec![n * 2024]
            }
        }
    }
}

fn digitize(n: usize) -> Vec<usize> {
    match n {
        n if n <= 9 => vec![n],
        n => {
            let mut m = digitize(n / 10);
            let mut f = vec![n % 10];
            f.append(&mut m);
            f
        }
    }
}

fn undigitize(n: Vec<usize>) -> usize {
    match &n[..] {
        [] => 0,
        [x] => *x,
        [x, xs @ ..] => x + (undigitize(xs.to_vec()) * 10),
    }
}

pub fn get_input() -> Vec<usize> {
    let mut file = File::open("input.txt").unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();
    buf.split_whitespace().map(|s| s.parse().unwrap()).collect()
}
