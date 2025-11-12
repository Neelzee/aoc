use crate::{BOARDING_PASS_ROW_MOD_COUNT, COLUMNS, MAX_COLUMNS, MAX_ROWS, ROWS};

pub fn run(input: &str) {
    let mut occupied_seat_ids = Vec::new();
    for boarding_pass in input.lines() {
        let mut back_row_ptr = MAX_ROWS;
        let mut front_row_ptr = 0;
        for row_mod in boarding_pass.chars().take(BOARDING_PASS_ROW_MOD_COUNT) {
            let diff = back_row_ptr - front_row_ptr;
            match row_mod {
                'F' => back_row_ptr -= diff / 2,
                'B' => front_row_ptr += diff / 2,
                _ => panic!("Unhandled row_mod: {row_mod}"),
            }
        }

        let mut back_col_ptr = MAX_COLUMNS;
        let mut front_col_ptr = 0;

        for column_mod in boarding_pass.chars().skip(BOARDING_PASS_ROW_MOD_COUNT) {
            let diff = back_col_ptr - front_col_ptr;
            match column_mod {
                'L' => back_col_ptr -= diff / 2,
                'R' => front_col_ptr += diff / 2,
                _ => panic!("Unhandled row_mod: {column_mod}"),
            }
        }

        occupied_seat_ids.push(
            (ROWS[front_row_ptr..back_row_ptr][0] + 1) * 8
                + (COLUMNS[front_col_ptr..back_col_ptr][0] + 1),
        );
    }

    occupied_seat_ids.sort();

    let mut all_seat_ids = Vec::new();

    for r in 0..MAX_ROWS {
        for c in 0..MAX_COLUMNS {
            all_seat_ids.push((r + 1) * 8 + (c + 1));
        }
    }

    for seat_id in all_seat_ids.iter() {
        match occupied_seat_ids.binary_search(seat_id) {
            Err(_) => match (
                occupied_seat_ids.binary_search(&(seat_id - 1)),
                occupied_seat_ids.binary_search(&(seat_id + 1)),
            ) {
                (Ok(_), Ok(_)) => {
                    println!("{seat_id}");
                    return;
                }
                _ => continue,
            },
            _ => continue,
        }
    }
}
