type Input<'a> = &'a str;

/// Parser for 2023 Day 01 (trebuchet)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input.trim()
}

/// Solver for part 1 of 2023 Day 01 (trebuchet)
///
/// # Panics
#[must_use]
pub fn part_1(input: Input) -> usize {
    input
        .split('\n')
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
pub fn part_2(input: Input) -> usize {
    part_1(
        [
            ("one", "o1e"),
            ("two", "t2o"),
            ("three", "t3e"),
            ("four", "f4r"),
            ("five", "f5e"),
            ("six", "s6x"),
            ("seven", "s7n"),
            ("eight", "e8t"),
            ("nine", "n9e"),
        ]
        .iter()
        .fold(input.to_string(), |acc, (from, to)| acc.replace(from, to))
        .as_str(),
    )
}
