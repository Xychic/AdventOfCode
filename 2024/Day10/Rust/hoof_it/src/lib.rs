use std::collections::{HashMap, HashSet, VecDeque};

type Input<'a> = Vec<Vec<u32>>;

/// Parser for 2024 Day 10 (`hoof_it`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| l.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect()
}

/// Solver for part 1 of 2024 Day 10 (`hoof_it`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut ans = 0;

    for (y, line) in input.iter().enumerate() {
        for (x, &c) in line.iter().enumerate() {
            if c != 0 {
                continue;
            }
            let mut queue = VecDeque::new();
            let mut seen = HashSet::new();
            queue.push_back((x, y));

            while let Some(curr @ (curr_x, curr_y)) = queue.pop_front() {
                if seen.contains(&curr) {
                    continue;
                }
                seen.insert(curr);
                if input[curr_y][curr_x] == 9 {
                    ans += 1;
                }
                for (new_x, new_y) in [
                    (curr_x, curr_y - 1),
                    (curr_x + 1, curr_y),
                    (curr_x, curr_y + 1),
                    (curr_x - 1, curr_y),
                ] {
                    if new_x < line.len()
                        && new_y < input.len()
                        && input[new_y][new_x] == input[curr_y][curr_x] + 1
                    {
                        queue.push_back((new_x, new_y));
                    }
                }
            }
        }
    }

    ans
}

/// Solver for part 2 of 2024 Day 10 (`hoof_it`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    input
        .iter()
        .enumerate()
        .map(|(y, line)| {
            line.iter()
                .enumerate()
                .filter(|(_, &v)| v == 0)
                .map(|(x, _)| get_paths(input, (x, y)))
                .sum::<usize>()
        })
        .sum()
}

fn get_paths(grid: &Input, (curr_x, curr_y): (usize, usize)) -> usize {
    let mut paths = 0;
    for (new_x, new_y) in [
        (curr_x, curr_y - 1),
        (curr_x + 1, curr_y),
        (curr_x, curr_y + 1),
        (curr_x - 1, curr_y),
    ] {
        if new_x < grid[curr_y].len()
            && new_y < grid.len()
            && grid[new_y][new_x] == grid[curr_y][curr_x] + 1
        {
            paths += match grid.get(new_y).unwrap().get(new_x).unwrap() {
                9 => 1,
                _ => get_paths(grid, (new_x, new_y)),
            }
        }
    }
    paths
}
