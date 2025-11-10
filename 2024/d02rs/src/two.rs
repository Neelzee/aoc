use crate::PuzzleInput;

pub fn run(inpt: PuzzleInput) -> usize {
    let (super_safe, second_chance): (Vec<_>, Vec<_>) = inpt
        .reports
        .into_iter()
        .partition(|r| crate::one::validate_report(r));

    super_safe.len()
        + second_chance
            .into_iter()
            .filter(|r| validate_report(r))
            .count()
}

pub fn validate_report(report: &[i32]) -> bool {
    let rep = &group(report);
    crate::one::validate_report(&check_again_diff(rep))
        || crate::one::validate_report(&check_again_decr(rep))
        || crate::one::validate_report(&check_again_incr(rep))
}

fn group(report: &[i32]) -> Vec<(i32, i32)> {
    let mut rep = report.to_vec();
    let mut prev = rep.remove(0);
    let mut vec = Vec::new();
    for cur in rep {
        vec.push((prev, cur));
        prev = cur;
    }
    vec
}

fn check_again_diff(rep: &[(i32, i32)]) -> Vec<i32> {
    let (min_diff, max_diff) = (1, 3);
    let mut report = rep.to_vec();
    let mut prev_tuple = report.remove(0);
    let mut check_again = Vec::new();
    report.reverse();
    while let Some(cur_tuple @ (prev, cur)) = report.pop() {
        let diff = (prev - cur).abs();
        if diff < min_diff || diff > max_diff {
            check_again.push((prev_tuple.0, prev));
            break;
        }
        check_again.push(prev_tuple);
        prev_tuple = cur_tuple;
    }

    report.reverse();
    check_again.append(&mut report);

    println!("diff: {:?}", check_again);

    check_again.into_iter().map(|t| t.0).collect()
}

fn check_again_incr(rep: &[(i32, i32)]) -> Vec<i32> {
    let mut report = rep.to_vec();
    let mut prev_tuple = report.remove(0);
    let mut check_again = Vec::new();
    report.reverse();
    while let Some(cur_tuple @ (prev, cur)) = report.pop() {
        if prev < cur {
            check_again.push((prev_tuple.0, prev));
            break;
        }
        check_again.push(prev_tuple);
        prev_tuple = cur_tuple;
    }

    report.reverse();
    check_again.append(&mut report);

    println!("incr: {:?}", check_again);
    check_again.into_iter().map(|t| t.0).collect()
}

fn check_again_decr(rep: &[(i32, i32)]) -> Vec<i32> {
    let mut report = rep.to_vec();
    let mut prev_tuple = report.remove(0);
    let mut check_again = Vec::new();
    report.reverse();
    while let Some(cur_tuple @ (prev, cur)) = report.pop() {
        if prev > cur {
            check_again.push((prev_tuple.0, prev));
            break;
        }
        check_again.push(prev_tuple);
        prev_tuple = cur_tuple;
    }

    report.reverse();
    check_again.append(&mut report);

    println!("decr: {:?}", check_again);
    check_again.into_iter().map(|t| t.0).collect()
}
