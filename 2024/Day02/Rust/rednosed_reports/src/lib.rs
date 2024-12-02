type Input<'a> = Vec<Vec<isize>>;

#[derive(Debug, PartialEq, Eq, Hash)]
enum State {
    Dec,
    Inc,
    Invalid,
}

/// Parser for 2024 Day 02 (`rednosed_reports`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| l.split(' ').map(|c| c.parse().unwrap()).collect())
        .collect()
}

/// Solver for part 1 of 2024 Day 02 (`rednosed_reports`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input.iter().filter(|xs| is_valid(xs)).count()
}

/// Solver for part 2 of 2024 Day 02 (`rednosed_reports`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    input
        .iter()
        .filter(|xs| (0..xs.len()).any(|i| is_valid(&[&xs[..i], &xs[i + 1..]].concat())))
        .count()
}

fn is_valid(xs: &[isize]) -> bool {
    xs.windows(2)
        .map(|xs| match xs[1] - xs[0] {
            1..=3 => State::Inc,
            -3..=-1 => State::Dec,
            _ => State::Invalid,
        })
        .collect::<Vec<_>>()
        .windows(2)
        .all(|xs| match (&xs[0], &xs[1]) {
            (State::Invalid, _) | (_, State::Invalid) => false,
            (a, b) => a == b,
        })
}
