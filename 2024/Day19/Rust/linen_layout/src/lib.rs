use std::collections::HashMap;

type Input<'a> = (Vec<&'a str>, Vec<&'a str>);

/// Parser for 2024 Day 19 (`linen_layout`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let (towels, designs) = input.trim().split_once("\n\n").unwrap();
    (towels.split(", ").collect(), designs.lines().collect())
}

/// Solver for part 1 of 2024 Day 19 (`linen_layout`)
///
/// # Panics
#[must_use]
pub fn part_1((towels, patterns): &Input) -> usize {
    fn is_possible(towels: &[&str], target: &str, known: &mut HashMap<String, bool>) -> bool {
        if let Some(&ans) = known.get(target) {
            return ans;
        } else if target.is_empty() {
            return true;
        }
        let ans = towels
            .iter()
            .filter(|&t| target.starts_with(t))
            .any(|t| is_possible(towels, &target[t.len()..], known));
        known.insert(target.to_owned(), ans);
        ans
    }

    patterns
        .iter()
        .filter(|p| is_possible(towels, p, &mut HashMap::new()))
        .count()
}

/// Solver for part 2 of 2024 Day 19 (`linen_layout`)
///
/// # Panics
#[must_use]
pub fn part_2((towels, patterns): &Input) -> usize {
    fn get_ways(towels: &[&str], target: &str, known: &mut HashMap<String, usize>) -> usize {
        if let Some(&ans) = known.get(target) {
            return ans;
        } else if target.is_empty() {
            return 1;
        }
        let ans = towels
            .iter()
            .filter(|&t| target.starts_with(t))
            .map(|t| get_ways(towels, &target[t.len()..], known))
            .sum();
        known.insert(target.to_owned(), ans);
        ans
    }

    patterns
        .iter()
        .map(|p| get_ways(towels, p, &mut HashMap::new()))
        .sum()
}
