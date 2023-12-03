use std::collections::HashMap;

type Input<'a> = Vec<Vec<char>>;

/// Parser for 2023 Day 03 (`gear_ratios`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input.trim().lines().map(|l| l.chars().collect()).collect()
}

/// Solver for part 1 of 2023 Day 03 (`gear_ratios`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut num = 0;
    let mut valid = false;
    let mut result = 0;
    for (y, line) in input.iter().enumerate() {
        for (x, c) in line.iter().enumerate() {
            if c.is_ascii_digit() {
                num = num * 10 + c.to_digit(10).unwrap();
                for (x2, y2) in [
                    (x.wrapping_sub(1), y.wrapping_sub(1)),
                    (x, y.wrapping_sub(1)),
                    (x + 1, y.wrapping_sub(1)),
                    (x.wrapping_sub(1), y),
                    (x + 1, y),
                    (x.wrapping_sub(1), y + 1),
                    (x, y + 1),
                    (x + 1, y + 1),
                ] {
                    if input
                        .get(y2)
                        .and_then(|row| row.get(x2))
                        .map(|&c| c != '.' && !c.is_ascii_digit())
                        == Some(true)
                    {
                        valid = true;
                    }
                }
            } else if num != 0 {
                if valid {
                    result += num as usize;
                }
                valid = false;
                num = 0;
            }
        }
        if num != 0 {
            if valid {
                result += num as usize;
            }
            valid = false;
            num = 0;
        }
    }
    result
}

/// Solver for part 2 of 2023 Day 03 (`gear_ratios`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut num = 0;
    let mut gear_pos = (0, 0);
    let mut res = 0;

    let mut gears = HashMap::new();

    for (y, line) in input.iter().enumerate() {
        for (x, c) in line.iter().enumerate() {
            if c.is_ascii_digit() {
                num = num * 10 + c.to_digit(10).unwrap();
                for (x2, y2) in [
                    (x.wrapping_sub(1), y.wrapping_sub(1)),
                    (x, y.wrapping_sub(1)),
                    (x + 1, y.wrapping_sub(1)),
                    (x.wrapping_sub(1), y),
                    (x + 1, y),
                    (x.wrapping_sub(1), y + 1),
                    (x, y + 1),
                    (x + 1, y + 1),
                ] {
                    if input.get(y2).and_then(|row| row.get(x2)).map(|&c| c == '*') == Some(true) {
                        gear_pos = (x2, y2);
                    }
                }
            } else if num != 0 {
                if gear_pos != (0, 0) {
                    if let Some(gear) = gears.get(&gear_pos) {
                        res += *gear * num as usize;
                    } else {
                        gears.insert(gear_pos, num as usize);
                    }
                }
                gear_pos = (0, 0);
                num = 0;
            }
        }
        if num != 0 {
            if gear_pos != (0, 0) {
                if let Some(gear) = gears.get(&gear_pos) {
                    res += *gear * num as usize;
                } else {
                    gears.insert(gear_pos, num as usize);
                }
            }
            gear_pos = (0, 0);
            num = 0;
        }
    }

    res
}
