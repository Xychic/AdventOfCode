type Input = Vec<Pair>;
type Pair = (usize, usize);

/// Parser for 2025 Day 02 (`gift_shop`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .split(',')
        .map(|x| {
            let (lower, upper) = x.split_once('-').unwrap();
            (lower.parse().unwrap(), upper.parse().unwrap())
        })
        .collect()
}

/// Solver for part 1 of 2025 Day 02 (`gift_shop`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .map(|&(a, b)| {
            (a..=b)
                .filter(|&x| {
                    let size = x.ilog10() + 1;
                    size.is_multiple_of(2) && x.is_multiple_of(10_usize.pow(size / 2) + 1)
                })
                .sum::<usize>()
        })
        .sum()
}

/// Solver for part 2 of 2025 Day 02 (`gift_shop`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    input
        .iter()
        .map(|&(a, b)| {
            (a..=b)
                .filter(|&x| match x.ilog10() + 1 {
                    1 => false,
                    2 => x.is_multiple_of(11),
                    3 => x.is_multiple_of(111),
                    4 => x.is_multiple_of(101),
                    5 => x.is_multiple_of(11111),
                    6 => x.is_multiple_of(10101) || x.is_multiple_of(1001),
                    7 => x.is_multiple_of(1111111),
                    8 => x.is_multiple_of(1010101) || x.is_multiple_of(10001),
                    9 => x.is_multiple_of(1001001),
                    10 => x.is_multiple_of(101010101) || x.is_multiple_of(100001),
                    _ => unreachable!(),
                })
                .sum::<usize>()
        })
        .sum()
}
