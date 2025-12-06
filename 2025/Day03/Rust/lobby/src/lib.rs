type Input = Vec<Vec<isize>>;

/// Parser for 2025 Day 03 (`lobby`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| isize::try_from(c.to_digit(10).unwrap()).unwrap())
                .collect::<Vec<_>>()
        })
        .collect()
}

fn solve(xs: &[isize], count: usize) -> isize {
    let mut ans = 0;
    let mut current_index = 0;
    for i in 0..count {
        let (index, val) = xs[current_index..(xs.len() - count + 1 + i)]
            .iter()
            .enumerate()
            .reduce(|acc, x| if x.1 > acc.1 { x } else { acc })
            .unwrap();
        current_index += index + 1;
        ans *= 10;
        ans += val;
    }
    ans
}

/// Solver for part 1 of 2025 Day 03 (`lobby`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> isize {
    input.iter().map(|xs| solve(xs, 2)).sum()
}

/// Solver for part 2 of 2025 Day 03 (`lobby`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> isize {
    input.iter().map(|xs| solve(xs, 12)).sum()
}
