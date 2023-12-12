use std::{collections::HashMap, iter::repeat};

type Input<'a> = Vec<(&'a str, Vec<usize>)>;

/// Parser for 2023 Day 12 (`hot_springs`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let (record, groups) = l.split_once(' ').unwrap();
            (
                record,
                groups.split(',').map(|x| x.parse().unwrap()).collect(),
            )
        })
        .collect()
}

fn solve(
    record: &str,
    groups: &[usize],
    run_len: usize,
    seen: &mut HashMap<(usize, usize, usize), usize>,
) -> usize {
    if let Some(&res) = seen.get(&(record.len(), groups.len(), run_len)) {
        return res;
    }
    if record.is_empty() {
        if (run_len == 0 && groups.is_empty()) || groups == [run_len] {
            return 1;
        }
        return 0;
    }
    let mut res = 0;

    for c in ["#", "."] {
        if ![c, "?"].contains(&&record[0..1]) {
            continue;
        }
        if c == "." && run_len > 0 && !groups.is_empty() && groups[0] == run_len {
            res += solve(&record[1..], &groups[1..], 0, seen);
        } else if c == "." && run_len == 0 {
            res += solve(&record[1..], groups, run_len, seen);
        } else if c == "#" {
            res += solve(&record[1..], groups, run_len + 1, seen);
        }
    }
    seen.insert((record.len(), groups.len(), run_len), res);
    res
}

/// Solver for part 1 of 2023 Day 12 (`hot_springs`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut seen = HashMap::new();
    input
        .iter()
        .map(|(record, groups)| {
            seen.clear();
            solve(record, groups, 0, &mut seen)
        })
        .sum()
}

/// Solver for part 2 of 2023 Day 12 (`hot_springs`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut seen = HashMap::new();
    input
        .iter()
        .map(|(record, groups)| {
            let record = repeat((*record).to_string())
                .take(5)
                .collect::<Vec<_>>()
                .join("?");
            let groups: Vec<_> = repeat(groups).take(5).flatten().copied().collect();
            seen.clear();
            solve(&record, &groups, 0, &mut seen)
        })
        .sum()
}
