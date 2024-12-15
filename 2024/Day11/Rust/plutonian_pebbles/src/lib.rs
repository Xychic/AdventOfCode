use std::collections::HashMap;

type Input<'a> = Vec<usize>;

/// Parser for 2024 Day 11 (`plutonian_pebbles`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .split(' ')
        .map(|x| x.parse().unwrap())
        .collect()
}

/// Solver for part 1 of 2024 Day 11 (`plutonian_pebbles`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut known = HashMap::new();
    input.iter().map(|x| solve(*x, 25, &mut known)).sum()
}

fn solve(number: usize, steps: usize, known: &mut HashMap<(usize, usize), usize>) -> usize {
    if let Some(&ans) = known.get(&(number, steps)) {
        return ans;
    }
    let ans;
    let num_str = format!("{number}");
    if steps == 0 {
        ans = 1;
    } else if number == 0 {
        ans = solve(1, steps - 1, known);
    } else if num_str.len() % 2 == 0 {
        ans = solve(
            num_str[..num_str.len() / 2].parse().unwrap(),
            steps - 1,
            known,
        ) + solve(
            num_str[num_str.len() / 2..].parse().unwrap(),
            steps - 1,
            known,
        );
    } else {
        ans = solve(number * 2024, steps - 1, known);
    }
    known.insert((number, steps), ans);
    ans
}

/// Solver for part 2 of 2024 Day 11 (`plutonian_pebbles`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut known = HashMap::new();
    input.iter().map(|x| solve(*x, 75, &mut known)).sum()
}
