pub fn run(input: &str) {
    let mut array: Vec<i32> = input.lines().flat_map(|s| s.parse::<i32>()).collect();

    array.sort();

    for x in array.iter() {
        if let Ok(i) = array.binary_search(&(2020 - x)) {
            println!("{}", x * array[i]);
            return;
        }
    }
}
