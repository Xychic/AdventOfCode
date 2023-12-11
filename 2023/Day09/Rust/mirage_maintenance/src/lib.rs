type Input<'a> = Vec<Vec<isize>>;

/// Parser for 2023 Day 09 (`mirage_maintenance`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| l.split(' ').map(|x| x.parse().unwrap()).collect())
        .collect()
}

fn get_next(sequence: &[isize]) -> isize {
    let diffs: Vec<_> = sequence.windows(2).map(|x| x[1] - x[0]).collect();
    if diffs.iter().all(|&x| x == 0) {
        return sequence[0];
    }
    sequence.last().unwrap() + get_next(&diffs)
}

fn get_prev(sequence: &[isize]) -> isize {
    let diffs: Vec<_> = sequence.windows(2).map(|x| x[1] - x[0]).collect();
    if diffs.iter().all(|&x| x == 0) {
        return sequence[0];
    }
    sequence[0] - get_prev(&diffs)
}

/// Solver for part 1 of 2023 Day 09 (`mirage_maintenance`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> isize {
    input.iter().map(|l| get_next(l)).sum()
}

/// Solver for part 2 of 2023 Day 09 (`mirage_maintenance`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> isize {
    input.iter().map(|l| get_prev(l)).sum()
}
