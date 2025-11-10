pub fn run(input: &str) {
    let mut array: Vec<i32> = input.lines().flat_map(|s| s.parse::<i32>()).collect();

    array.sort();

    #[cfg(feature = "opt")]
    opt(&array);

    #[cfg(not(feature = "opt"))]
    de_opt(&array);
}

#[cfg(not(feature = "opt"))]
pub fn de_opt(xs: &[i32]) {
    'outer: for i in xs {
        for j in xs {
            for n in xs {
                if i + j + n == 2020 {
                    println!("{}", i * j * n);
                    break 'outer;
                }
            }
        }
    }
}

#[cfg(feature = "opt")]
pub fn opt(xs: &[i32]) {
    for x in xs {
        if let Some((y, z)) = two(2020 - x, xs) {
            println!("{}", x * y * z);
            break;
        }
    }
}

pub fn two(x: i32, xs: &[i32]) -> Option<(i32, i32)> {
    for y in xs {
        if let Ok(i) = xs.binary_search(&(x - y)) {
            return Some((*y, xs[i]));
        }
    }
    None
}