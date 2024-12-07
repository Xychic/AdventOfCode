type Input<'a> = Vec<(usize, Vec<usize>)>;

/// Parser for 2024 Day 07 (`bridge_repair`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let (a, b) = l.split_once(": ").unwrap();
            (
                a.parse().unwrap(),
                b.split(' ').rev().map(|x| x.parse().unwrap()).collect(),
            )
        })
        .collect()
}

fn can_be_made(target: usize, nums: &[usize], part_2: bool) -> bool {
    if nums.len() == 1 {
        return nums[0] == target;
    }
    let current = nums[0];
    let next = &nums[1..];
    let current_string = current.to_string();

    (target % current == 0 && can_be_made(target / current, next, part_2))
        || (target > current && can_be_made(target - current, next, part_2))
        || (part_2
            && target.to_string().ends_with(&current_string)
            && can_be_made(
                (target - current) / 10_usize.pow(u32::try_from(current_string.len()).unwrap()),
                next,
                part_2,
            ))
}

/// Solver for part 1 of 2024 Day 07 (`bridge_repair`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    // dbg!(input);
    input
        .iter()
        .filter(|(a, b)| can_be_made(*a, b, false))
        .map(|(a, _)| a)
        .sum()
}

/// Solver for part 2 of 2024 Day 07 (`bridge_repair`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    input
        .iter()
        .filter(|(a, b)| can_be_made(*a, b, true))
        .map(|(a, _)| a)
        .sum()
}
