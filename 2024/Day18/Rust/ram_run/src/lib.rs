use std::collections::{HashMap, HashSet, VecDeque};

type Input<'a> = Vec<Point>;
type Point = (usize, usize);

/// Parser for 2024 Day 18 (`ram_run`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let (a, b) = l.split_once(',').unwrap();
            (a.parse().unwrap(), b.parse().unwrap())
        })
        .collect()
}

/// Solver for part 1 of 2024 Day 18 (`ram_run`)
///
/// # Panics
#[must_use]
pub fn part_1<const W: usize, const H: usize, const T: usize>(input: &Input) -> usize {
    let mut grid = HashMap::new();
    for y in 0..=H {
        for x in 0..=W {
            grid.insert((x, y), '.');
        }
    }
    for &p in input.iter().take(T) {
        grid.insert(p, '#');
    }
    get_path::<W, H>(&grid).unwrap().len() - 1
}

fn get_path<const W: usize, const H: usize>(grid: &HashMap<Point, char>) -> Option<Vec<Point>> {
    let mut queue = VecDeque::new();
    let mut seen = HashSet::new();
    queue.push_back(((0, 0), vec![(0, 0)]));
    while let Some((pos @ (x, y), path)) = queue.pop_front() {
        if pos == (W, H) {
            return Some(path);
        }
        if seen.contains(&pos) {
            continue;
        }
        seen.insert(pos);
        for new_pos in [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)] {
            if grid.get(&new_pos) == Some(&'.') {
                let mut new_path = path.clone();
                new_path.push(new_pos);
                queue.push_back((new_pos, new_path));
            }
        }
    }
    None
}

/// Solver for part 2 of 2024 Day 18 (`ram_run`)
///
/// # Panics
#[must_use]
pub fn part_2<const W: usize, const H: usize, const T: usize>(input: &Input) -> String {
    let mut grid = HashMap::new();
    for y in 0..=H {
        for x in 0..=W {
            grid.insert((x, y), '.');
        }
    }
    for &p in input.iter().take(T) {
        grid.insert(p, '#');
    }

    let mut path: HashSet<_> = get_path::<W, H>(&grid).unwrap().iter().copied().collect();
    for &pos @ (x, y) in input.iter().skip(T) {
        grid.insert(pos, '#');
        if path.contains(&pos) {
            if let Some(new_path) = get_path::<W, H>(&grid) {
                path = new_path.iter().copied().collect();
            } else {
                return format!("{x},{y}");
            }
        }
    }
    unreachable!()
}
