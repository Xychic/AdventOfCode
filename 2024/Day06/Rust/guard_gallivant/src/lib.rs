use std::collections::HashSet;

type Input<'a> = ((usize, usize), HashSet<(usize, usize)>, (usize, usize));

/// Parser for 2024 Day 06 (`guard_gallivant`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut guard = None;
    let mut max_x = 0;
    let mut max_y = 0;
    let mut obstacles = HashSet::new();

    for (y, line) in input.trim().lines().enumerate() {
        max_y = max_y.max(y);
        for (x, c) in line.chars().enumerate() {
            max_x = max_x.max(x);
            match c {
                '#' => {
                    obstacles.insert((x, y));
                }
                '^' => {
                    guard = Some((x, y));
                }
                _ => continue,
            }
        }
    }
    (guard.unwrap(), obstacles, (max_x, max_y))
}

/// Solver for part 1 of 2024 Day 06 (`guard_gallivant`)
///
/// # Panics
#[must_use]
pub fn part_1((guard, obstacles, (max_x, max_y)): &Input) -> usize {
    let mut seen = HashSet::new();
    let mut dir_index = 0;

    let (mut guard_x, mut guard_y) = guard;
    seen.insert((guard_x, guard_y));
    while guard_x < *max_x && guard_y < *max_y {
        let new_pos = [
            (guard_x, guard_y - 1),
            (guard_x + 1, guard_y),
            (guard_x, guard_y + 1),
            (guard_x - 1, guard_y),
        ][dir_index];
        if obstacles.contains(&new_pos) {
            dir_index = (dir_index + 1) % 4;
            continue;
        }
        (guard_x, guard_y) = new_pos;
        seen.insert(new_pos);
    }
    seen.len()
}

/// Solver for part 2 of 2024 Day 06 (`guard_gallivant`)
///
/// # Panics
#[must_use]
pub fn part_2((guard, obstacles, (max_x, max_y)): &Input) -> usize {
    let mut seen = HashSet::new();
    let mut dir_index = 0;

    let (mut guard_x, mut guard_y) = guard;
    seen.insert((guard_x, guard_y));
    while guard_x < *max_x && guard_y < *max_y {
        let new_pos = [
            (guard_x, guard_y - 1),
            (guard_x + 1, guard_y),
            (guard_x, guard_y + 1),
            (guard_x - 1, guard_y),
        ][dir_index];
        if obstacles.contains(&new_pos) {
            dir_index = (dir_index + 1) % 4;
            continue;
        }
        (guard_x, guard_y) = new_pos;
        seen.insert(new_pos);
    }

    seen.iter()
        .filter(|&&pos| {
            let mut new_o = obstacles.clone();
            new_o.insert(pos);
            is_loop(&(*guard, new_o, (*max_x, *max_y)))
        })
        .count()
}

fn is_loop((guard, obstacles, (max_x, max_y)): &Input) -> bool {
    let mut seen = HashSet::new();
    let mut dir_index = 0;

    let (mut guard_x, mut guard_y) = guard;
    seen.insert((guard_x, guard_y, dir_index));
    while guard_x < *max_x && guard_y < *max_y {
        let new_pos = [
            (guard_x, guard_y - 1),
            (guard_x + 1, guard_y),
            (guard_x, guard_y + 1),
            (guard_x - 1, guard_y),
        ][dir_index];
        if obstacles.contains(&new_pos) {
            dir_index = (dir_index + 1) % 4;
            continue;
        }
        (guard_x, guard_y) = new_pos;
        let new_state = (guard_x, guard_y, dir_index);
        if seen.contains(&new_state) {
            return true;
        }
        seen.insert(new_state);
    }
    false
}
