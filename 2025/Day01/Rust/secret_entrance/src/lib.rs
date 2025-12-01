type Input = Vec<i64>;

/// Parser for 2025 Day 01 (`secret_entrance`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| match l.chars().next().unwrap() {
            'L' => -l[1..].parse::<i64>().unwrap(),
            'R' => l[1..].parse().unwrap(),
            _ => unreachable!(),
        })
        .collect()
}

/// Solver for part 1 of 2025 Day 01 (`secret_entrance`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .fold((50, 0), |(pos, count), &dir| {
            let new_pos = (pos + dir) % 100;
            (new_pos, if new_pos == 0 { count + 1 } else { count })
        })
        .1
}

/// Solver for part 2 of 2025 Day 01 (`secret_entrance`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut pos = 50;
    let mut count = 0;

    for &d in input {
        let mut c = 0;
        let delta = d.signum();
        
        for _ in 0..d.abs() {
            pos = (pos + delta) % 100;
            if pos == 0 {
                c += 1
            }
        }
        count += c;
    }
    count
}
