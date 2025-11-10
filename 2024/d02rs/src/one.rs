use crate::PuzzleInput;

pub fn run(inpt: PuzzleInput) -> usize {
    inpt.reports
        .into_iter()
        .filter(|r| validate_report(r))
        .count()
}

pub fn validate_report(report: &[i32]) -> bool {
    let (min_diff, max_diff) = (1, 3);
    let mut rep = report.to_vec();
    let mut prev = rep.remove(0);
    rep.reverse();
    let mut incr = true;
    let mut decr = true;
    while let Some(cur) = rep.pop() {
        if incr && prev > cur {
            incr = false;
        }
        if decr && prev < cur {
            decr = false;
        }
        let diff = (prev - cur).abs();
        if diff < min_diff || diff > max_diff {
            return false;
        }
        if !incr && !decr {
            return false;
        }
        prev = cur;
    }

    incr || decr
}
