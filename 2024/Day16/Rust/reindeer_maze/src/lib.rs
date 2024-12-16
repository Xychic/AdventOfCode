use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap, HashSet},
};

use itertools::Itertools;

type Input<'a> = (HashMap<Point, char>, Point, Point);
type Point = (usize, usize);

/// Parser for 2024 Day 16 (`reindeer_maze`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut start = None;
    let mut end = None;
    let mut map = HashMap::new();
    for (y, line) in input.trim().lines().enumerate() {
        for (x, c) in line.char_indices() {
            map.insert(
                (x, y),
                match c {
                    'S' => {
                        start = Some((x, y));
                        '.'
                    }
                    'E' => {
                        end = Some((x, y));
                        '.'
                    }
                    c => c,
                },
            );
        }
    }
    (map, start.unwrap(), end.unwrap())
}

/// Solver for part 1 of 2024 Day 16 (`reindeer_maze`)
///
/// # Panics
#[must_use]
pub fn part_1((map, start, end): &Input) -> usize {
    let mut seen = HashSet::new();
    let mut queue = BinaryHeap::new();

    queue.push(Reverse((0, *start, 1)));

    while let Some(Reverse((dist, pos @ (x, y), heading))) = queue.pop() {
        if pos == *end {
            return dist;
        }
        if seen.contains(&(pos, heading)) {
            continue;
        }
        seen.insert((pos, heading));

        let new_pos = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)][heading];
        if map.get(&new_pos) == Some(&'.') {
            queue.push(Reverse((dist + 1, new_pos, heading)));
        }
        queue.push(Reverse((dist + 1000, pos, (heading + 3) % 4)));
        queue.push(Reverse((dist + 1000, pos, (heading + 1) % 4)));
    }

    unreachable!()
}

/// Solver for part 2 of 2024 Day 16 (`reindeer_maze`)
///
/// # Panics
#[must_use]
pub fn part_2((map, start, end): &Input) -> usize {
    let mut dist_to_end = HashMap::new();
    let mut queue = BinaryHeap::new();
    let mut path_len = None;

    queue.push(Reverse((0, *start, 1)));

    while let Some(Reverse((dist, pos @ (x, y), heading))) = queue.pop() {
        if pos == *end && path_len.is_none() {
            path_len = Some(dist);
        }
        if dist_to_end.contains_key(&(pos, heading)) {
            continue;
        }
        dist_to_end.insert((pos, heading), dist);

        let new_pos = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)][heading];
        if map.get(&new_pos) == Some(&'.') {
            queue.push(Reverse((dist + 1, new_pos, heading)));
        }
        queue.push(Reverse((dist + 1000, pos, (heading + 3) % 4)));
        queue.push(Reverse((dist + 1000, pos, (heading + 1) % 4)));
    }


    let mut dist_to_start = HashMap::new();
    for i in 0..4 {
        queue.push(Reverse((0, *end, i)));
    }
    while let Some(Reverse((dist, pos @ (x, y), heading))) = queue.pop() {
        if pos == *end && path_len.is_none() {
            path_len = Some(dist);
        }
        if dist_to_start.contains_key(&(pos, heading)) {
            continue;
        }
        dist_to_start.insert((pos, heading), dist);

        // Invert signs to walk opposite to heading
        let new_pos = [(x, y + 1), (x - 1, y), (x, y - 1), (x + 1, y)][heading];
        if map.get(&new_pos) == Some(&'.') {
            queue.push(Reverse((dist + 1, new_pos, heading)));
        }
        queue.push(Reverse((dist + 1000, pos, (heading + 3) % 4)));
        queue.push(Reverse((dist + 1000, pos, (heading + 1) % 4)));
    }

    dist_to_end
        .iter()
        .filter_map(|(key @ (pos, _), dist_1)| {
            if let Some(dist_2) = dist_to_start.get(key) {
                if Some(dist_2 + dist_1) == path_len {
                    return Some(pos);
                }
            }
            None
        })
        .unique()
        .count()
}
