use std::collections::HashMap;

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

fn solve(xs: &[isize], count: u32, seen: &mut HashMap<(usize, u32), isize>) -> isize {
    if xs.is_empty() {
        if count == 0 {
            return 0;
        }
        return isize::MIN;
    }
    if count == 0 {
        return 0;
    }
    if let Some(&ans) = seen.get(&(xs.len(), count)) {
        return ans;
    }
    let ans = solve(&xs[1..], count, seen)
        .max(xs[0] * 10_isize.pow(count - 1) + solve(&xs[1..], count - 1, seen));
    seen.insert((xs.len(), count), ans);
    ans
}

/// Solver for part 1 of 2025 Day 03 (`lobby`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> isize {
    input
        .iter()
        .map(|xs| solve(xs, 2, &mut HashMap::new()))
        .sum()
}

/// Solver for part 2 of 2025 Day 03 (`lobby`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> isize {
    input
        .iter()
        .map(|xs| solve(xs, 12, &mut HashMap::new()))
        .sum()
}
