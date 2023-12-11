use std::collections::HashMap;

use num::integer::lcm;

type Input<'a> = (&'a str, HashMap<String, (&'a str, &'a str)>);

/// Parser for 2023 Day 08 (`haunted_wasteland`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let (cycle, map_str) = input.trim().split_once("\n\n").unwrap();
    (
        cycle,
        map_str
            .lines()
            .map(|line| {
                let (from, to) = line.split_once(" = ").unwrap();
                let (l, r) = to[1..to.len() - 1].split_once(", ").unwrap();
                (from.to_owned(), (l, r))
            })
            .collect(),
    )
}

/// Solver for part 1 of 2023 Day 08 (`haunted_wasteland`)
///
/// # Panics
#[must_use]
pub fn part_1((cycle, map): &Input) -> usize {
    let mut current = "AAA";
    for (i, step) in cycle.chars().cycle().enumerate() {
        if current == "ZZZ" {
            return i;
        }
        let next = map.get(current).unwrap();
        current = if step == 'L' { next.0 } else { next.1 };
    }
    unreachable!()
}

/// Solver for part 2 of 2023 Day 08 (`haunted_wasteland`)
///
/// # Panics
#[must_use]
pub fn part_2((cycle, map): &Input) -> usize {
    let mut positions: Vec<_> = map
        .keys()
        .filter(|k| k.ends_with('A'))
        .map(std::string::String::as_str)
        .collect();
    let ghost_count = positions.len();
    let mut cycle_lens = Vec::with_capacity(ghost_count);

    for (i, step) in cycle.chars().cycle().enumerate() {
        for p in &mut positions {
            if p.ends_with('Z') {
                cycle_lens.push(i);
                if cycle_lens.len() == ghost_count {
                    return cycle_lens.iter().fold(cycle_lens[0], |acc, &x| lcm(acc, x));
                }
            }
            let &mut p_ref = p;
            let next = map.get(p_ref).unwrap();
            *p = if step == 'L' { next.0 } else { next.1 };
        }
    }

    todo!()
}
