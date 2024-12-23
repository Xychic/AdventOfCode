use std::collections::{HashMap, HashSet, VecDeque};

type Input<'a> = HashMap<&'a str, HashSet<&'a str>>;

/// Parser for 2024 Day 23 (`lan_party`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut parsed = HashMap::new();
    for line in input.trim().lines() {
        let (from, to) = line.split_once('-').unwrap();
        parsed.entry(from).or_insert(HashSet::new()).insert(to);
        parsed.entry(to).or_insert(HashSet::new()).insert(from);
    }
    parsed
}

/// Solver for part 1 of 2024 Day 23 (`lan_party`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut triples = HashSet::new();
    let mut seen = HashSet::new();
    for (from, to_set) in input {
        if !from.starts_with('t') {
            continue;
        }
        for to in to_set {
            if seen.contains(to) {
                continue;
            }
            for dest in input.get(to).unwrap() {
                if seen.contains(dest) {
                    continue;
                }
                let set = input.get(dest).unwrap();
                if set.contains(from) {
                    let mut to_sort = [*from, to, dest];
                    to_sort.sort_unstable();
                    triples.insert(to_sort.join(","));
                }
            }
        }
        seen.insert(from);
    }
    triples.len()
}

/// Solver for part 2 of 2024 Day 23 (`lan_party`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> String {
    let mut queue = VecDeque::new();
    for (k, v) in input {
        let mut new_set = v.clone();
        new_set.insert(*k);
        queue.push_back(new_set);
    }
    while let Some(lan) = queue.pop_front() {
        if lan.iter().all(|a| {
            lan.iter()
                .filter(|b| a != *b)
                .all(|b| input.get(a).unwrap().contains(b))
        }) {
            let mut to_sort: Vec<_> = lan.iter().copied().collect();
            to_sort.sort_unstable();
            return to_sort.join(",");
        }
        for pc in &lan {
            queue.push_back(lan.iter().filter(|x| *x != pc).copied().collect());
        }
    }
    unreachable!()
}
