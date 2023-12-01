type Input<'a> = Vec<&'a str>;

/// Parser for 2023 Day 01 (trebuchet)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input.trim().split('\n').collect()
}

/// Solver for part 1 of 2023 Day 01 (trebuchet)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .map(|l| {
            let digits: Vec<_> = l
                .chars()
                .filter(|c| c.is_numeric())
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect();
            digits[0] * 10 + digits.last().unwrap()
        })
        .sum()
}

/// Solver for part 2 of 2023 Day 01 (trebuchet)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let digit_vals = [
        ("1", 1),
        ("2", 2),
        ("3", 3),
        ("4", 4),
        ("5", 5),
        ("6", 6),
        ("7", 7),
        ("8", 8),
        ("9", 9),
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9),
    ];

    input
        .iter()
        .map(|l| {
            let mut digits: Vec<_> = digit_vals
                .iter()
                .flat_map(|(n, v)| l.match_indices(n).map(|(i, _)| (i, *v)))
                .collect();
            digits.sort_by_key(|(i, _)| *i);
            digits[0].1 * 10 + digits.last().unwrap().1
        })
        .sum()
}
