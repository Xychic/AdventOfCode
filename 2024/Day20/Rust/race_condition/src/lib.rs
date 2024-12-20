use std::collections::{HashMap, VecDeque};

type Input<'a> = (HashMap<Point, char>, Point, Point);
type Point = (usize, usize);

/// Parser for 2024 Day 20 (`race_condition`)
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
                    _ => c,
                },
            );
        }
    }
    (map, start.unwrap(), end.unwrap())
}

/// Solver for part 1 of 2024 Day 20 (`race_condition`)
///
/// # Panics
#[must_use]
pub fn part_1<const T: usize>((map, start, end): &Input) -> usize {
    let mut seen = HashMap::new();
    let mut queue = VecDeque::new();
    queue.push_back((*start, 0_usize));

    while let Some((p @ (x, y), d)) = queue.pop_front() {
        if seen.contains_key(&p) {
            continue;
        }
        seen.insert(p, d);
        if p == *end {
            continue;
        }
        for new_point in [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)] {
            if map.get(&new_point) == Some(&'.') {
                queue.push_back((new_point, d + 1));
            }
        }
    }


    let sorted: Vec<_> = {
        let mut items: Vec<_> = seen.iter().collect();
        items.sort_by_key(|(_, &d)| d);
        items.iter().map(|(&p,_)| p).collect()
    };
    let mut ans = 0;
    for (i, a@&(a_x, a_y)) in sorted.iter().enumerate() {
        for b@&(b_x, b_y) in sorted.iter().skip(i+1) {
            let jump = a_x.abs_diff(b_x) + a_y.abs_diff(b_y);
            if jump > 2 {
                continue;
            }
            let saved = seen
                .get(b)
                .unwrap()
                .saturating_sub(seen.get(a).unwrap() + jump);
            ans += usize::from(saved >= T);
        }
    }
    ans
}

/// Solver for part 2 of 2024 Day 20 (`race_condition`)
///
/// # Panics
#[must_use]
pub fn part_2<const T: usize>((map, start, end): &Input) -> usize {
    let mut seen = HashMap::new();
    let mut queue = VecDeque::new();
    queue.push_back((*start, 0_usize));

    while let Some((p @ (x, y), d)) = queue.pop_front() {
        if seen.contains_key(&p) {
            continue;
        }
        seen.insert(p, d);
        if p == *end {
            continue;
        }
        for new_point in [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)] {
            if map.get(&new_point) == Some(&'.') {
                queue.push_back((new_point, d + 1));
            }
        }
    }

    let sorted: Vec<_> = {
        let mut items: Vec<_> = seen.iter().collect();
        items.sort_by_key(|(_, &d)| d);
        items.iter().map(|(&p,_)| p).collect()
    };
    let mut ans = 0;
    for (i, a@&(a_x, a_y)) in sorted.iter().enumerate() {
        for b@&(b_x, b_y) in sorted.iter().skip(i+1) {
            let jump = a_x.abs_diff(b_x) + a_y.abs_diff(b_y);
            if jump > 2 {
                continue;
            }
            let saved = seen
                .get(b)
                .unwrap()
                .saturating_sub(seen.get(a).unwrap() + jump);
            ans += usize::from(saved >= T);
        }
    }
    ans
}
