use std::collections::{HashMap, HashSet, VecDeque};

type Input<'a> = (Vec<Vec<char>>, (usize, usize));

/// Parser for 2023 Day 21 (`step_counter`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut start = (0, 0);
    (
        input
            .trim()
            .lines()
            .enumerate()
            .map(|(y, l)| {
                l.char_indices()
                    .map(|(x, c)| {
                        if c == 'S' {
                            start = (x, y);
                            '.'
                        } else {
                            c
                        }
                    })
                    .collect()
            })
            .collect(),
        start,
    )
}

/// Solver for part 1 of 2023 Day 21 (`step_counter`)
///
/// # Panics
#[must_use]
pub fn part_1((map, start): &Input, steps: usize) -> usize {
    let mut queue = VecDeque::with_capacity(steps);
    let mut seen = HashMap::new();
    seen.insert(*start, 0);
    queue.push_back((*start, 0));
    while let Some(((x, y), dist)) = queue.pop_front() {
        if dist > steps {
            continue;
        }
        for (nx, ny) in [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)] {
            if nx > map[0].len()
                || ny > map.len()
                || map[ny][nx] != '.'
                || seen.contains_key(&(nx, ny))
            {
                continue;
            }
            queue.push_back(((nx, ny), dist + 1));
            seen.insert((nx, ny), dist + 1);
        }
    }

    seen.values()
        .filter(|&&x| x <= steps && x % 2 == steps % 2)
        .count()
}

/// Solver for part 2 of 2023 Day 21 (`step_counter`)
///
/// # Panics
#[must_use]
pub fn part_2((map, start): &Input, steps: usize) -> usize {
    let y_size = map.len();
    let x_size = map[0].len();
    let max_grid = 5;

    let mut distances = HashMap::new();
    let mut queue = VecDeque::new();

    queue.push_back((*start, (0_i32, 0_i32), 0));
    distances.insert((*start, (0, 0)), 0);

    while let Some(((x, y), (grid_x, grid_y), dist)) = queue.pop_front() {
        if dist >= steps || grid_x.abs() > max_grid || grid_y.abs() > max_grid {
            continue;
        }
        for (mut nx, mut ny) in [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)] {
            let mut ngrid_x = grid_x;
            let mut ngrid_y = grid_y;
            if nx == x_size {
                ngrid_x += 1;
                nx = 0;
            } else if nx == usize::MAX {
                ngrid_x -= 1;
                nx = x_size - 1;
            }
            if ny == y_size {
                ngrid_y += 1;
                ny = 0;
            } else if ny == usize::MAX {
                ngrid_y -= 1;
                ny = y_size - 1;
            }
            if map[ny][nx] != '.' || distances.contains_key(&((nx, ny), (ngrid_x, ngrid_y))) {
                continue;
            }
            queue.push_back(((nx, ny), (ngrid_x, ngrid_y), dist + 1));
            distances.insert(((nx, ny), (ngrid_x, ngrid_y)), dist + 1);
        }
    }

    let mut ans = 0;
    // let mut seen_edge_case = HashMap::new();
    let mut seen_corner_case = HashMap::new();
    for (y, line) in map.iter().enumerate() {
        for (x, &c) in line.iter().enumerate() {
            if c == '#' {
                continue;
            }
            for grid_y in -max_grid..=max_grid {
                for grid_x in -max_grid..=max_grid {
                    if let Some(&dist) = distances.get(&((x, y), (grid_x, grid_y))) {
                        if grid_x.abs() == max_grid && grid_y.abs() == max_grid {
                            if let Some(res) = seen_corner_case.get(&dist) {
                                ans += res;
                            } else {
                                let mut res = 0;
                                let repeats = (steps - dist) / x_size;
                                for grid_delta in 0..=repeats {
                                    let new_dist = dist + (grid_delta * x_size);
                                    if new_dist < steps && new_dist % 2 == steps % 2 {
                                        res += grid_delta + 1;
                                    }
                                }
                                seen_corner_case.insert(dist, res);
                                ans += res;
                            }
                        } else if grid_x.abs() == max_grid || grid_y.abs() == max_grid {
                            // if let Some(res) = seen_edge_case.get(&dist) {
                            //     ans += res;
                            // } else {
                            //     let mut res = 0;
                            //     let repeats = (steps - dist) / x_size;
                            //     for grid_delta in 0..=repeats {
                            //         let new_dist = dist + (grid_delta * x_size);
                            //         if new_dist < steps && new_dist % 2 == steps % 2 {
                            //             res += 1;
                            //         }
                            //     }
                            //     seen_edge_case.insert(dist, res);
                            //     ans += res;
                            // }
                        } else if dist <= steps && dist % 2 == steps % 2 {
                            ans += 1;
                        }
                    }
                }
            }
        }
    }
    ans
}
