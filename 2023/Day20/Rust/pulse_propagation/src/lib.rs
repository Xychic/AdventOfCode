use std::collections::{HashMap, HashSet, VecDeque};

type Input<'a> = HashMap<&'a str, Vec<&'a str>>;

/// Parser for 2023 Day 20 (`pulse_propagation`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut typings = HashMap::new();
    let mut parsed: Input = input
        .trim()
        .lines()
        .map(|l| {
            let (name, to) = l.split_once(" -> ").unwrap();
            (
                match &name[..1] {
                    "%" | "&" => {
                        typings.insert(&name[1..], name);
                        name
                    }
                    _ => name,
                },
                to.split(", ").collect(),
            )
        })
        .collect();

    for dests in &mut parsed.values_mut() {
        for item in dests {
            if typings.contains_key(item) {
                *item = typings.get(item).unwrap();
            };
        }
    }

    parsed
}

/// Solver for part 1 of 2023 Day 20 (`pulse_propagation`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut conj_state = HashMap::new();
    for (k, dests) in input {
        for d in dests {
            if d.starts_with('&') {
                conj_state
                    .entry(d)
                    .or_insert(HashMap::new())
                    .insert(k, false);
            }
        }
    }
    let mut flip_state: HashMap<_, _> = input
        .keys()
        .filter(|x| x.starts_with('%'))
        .map(|x| (x, false))
        .collect();
    let mut low_count = 0;
    let mut high_count = 0;
    let mut queue = VecDeque::new();

    for _ in 0..1000 {
        queue.push_back(("broadcaster", "", false));
        while let Some((module, from, state)) = queue.pop_front() {
            if state {
                high_count += 1;
            } else {
                low_count += 1;
            }

            match &module[..1] {
                "%" => {
                    if state {
                        continue;
                    }
                    let new_state = flip_state.get_mut(&module).unwrap();
                    *new_state = !*new_state;
                    for dest in input.get(module).unwrap() {
                        queue.push_back((dest, module, *new_state));
                    }
                }
                "&" => {
                    *conj_state.get_mut(&module).unwrap().get_mut(&from).unwrap() = state;
                    let new_state = !conj_state.get(&module).unwrap().values().all(|&x| x);
                    for dest in input.get(&module).unwrap() {
                        queue.push_back((dest, module, new_state));
                    }
                }
                _ => {
                    for dest in input.get(module).unwrap_or(&Vec::new()) {
                        queue.push_back((dest, module, state));
                    }
                }
            }
        }
    }

    low_count * high_count
}

/// Solver for part 2 of 2023 Day 20 (`pulse_propagation`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut conj_state = HashMap::new();
    for (k, dests) in input {
        for d in dests {
            if d.starts_with('&') {
                conj_state
                    .entry(d)
                    .or_insert(HashMap::new())
                    .insert(k, false);
            }
        }
    }
    let mut flip_state: HashMap<_, _> = input
        .keys()
        .filter(|x| x.starts_with('%'))
        .map(|x| (x, false))
        .collect();

    let mut targets = HashSet::new();
    targets.insert("rx");
    while targets.len() == 1 {
        targets = input
            .keys()
            .filter(|&k| {
                input
                    .get(k)
                    .unwrap()
                    .contains(targets.iter().next().unwrap())
            })
            .copied()
            .collect();
    }

    let mut cycles = Vec::with_capacity(targets.len());
    let mut seen = HashMap::with_capacity(targets.len());
    let mut queue = VecDeque::new();

    for i in 0_usize.. {
        queue.push_back(("broadcaster", "", false));
        while let Some((module, from, state)) = queue.pop_front() {
            if !state && targets.contains(module) {
                if let Some(t) = seen.get(&module) {
                    cycles.push(i - t);
                    if cycles.len() == targets.len() {
                        return cycles.iter().fold(1, |acc, &x| num::integer::lcm(acc, x));
                    }
                }
                seen.insert(module, i);
            }

            match &module[..1] {
                "%" => {
                    if state {
                        continue;
                    }
                    let new_state = flip_state.get_mut(&module).unwrap();
                    *new_state = !*new_state;
                    for dest in input.get(module).unwrap() {
                        queue.push_back((dest, module, *new_state));
                    }
                }
                "&" => {
                    *conj_state.get_mut(&module).unwrap().get_mut(&from).unwrap() = state;
                    let new_state = !conj_state.get(&module).unwrap().values().all(|&x| x);
                    for dest in input.get(&module).unwrap() {
                        queue.push_back((dest, module, new_state));
                    }
                }
                _ => {
                    for dest in input.get(module).unwrap_or(&Vec::new()) {
                        queue.push_back((dest, module, state));
                    }
                }
            }
        }
    }
    unreachable!()
}
