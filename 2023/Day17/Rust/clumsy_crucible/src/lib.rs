use std::{cmp::Reverse, collections::HashSet};

use priority_queue::PriorityQueue;

type Input<'a> = Vec<Vec<usize>>;

const UP: usize = 0;
const RIGHT: usize = 1;
const DOWN: usize = 2;
const LEFT: usize = 3;

/// Parser for 2023 Day 17 (`clumsy_crucible`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect()
        })
        .collect()
}

fn solve<const A: usize, const B: usize>(input: &Input) -> usize {
    let mut queue = PriorityQueue::new();
    let mut seen = HashSet::new();
    let end = (input[0].len() - 1, input.len() - 1);
    queue.push(((0, 0), DOWN, 0), Reverse(0));
    queue.push(((0, 0), RIGHT, 0), Reverse(0));

    while let Some((((x, y), dir, dist), _)) = queue.pop() {
        if (x, y) == end {
            return dist;
        }
        if seen.contains(&((x, y), dir)) {
            continue;
        }
        seen.insert(((x, y), dir));

        for new_dir in [[RIGHT, LEFT], [UP, DOWN]][dir % 2] {
            for i in A..=B {
                let nx = [x, x + i, x, x - i][new_dir];
                let ny = [y - i, y, y + i, y][new_dir];
                if nx >= input[0].len() || ny >= input.len() {
                    continue;
                }
                let cost: usize = (1..=i)
                    .map(|j| {
                        let nx = [x, x + j, x, x - j][new_dir];
                        let ny = [y - j, y, y + j, y][new_dir];
                        input[ny][nx]
                    })
                    .sum();
                queue.push(
                    ((nx, ny), new_dir, dist + cost),
                    Reverse(100_000 * (dist + cost) + queue.len()),
                );
            }
        }
    }

    unreachable!()
}

/// Solver for part 1 of 2023 Day 17 (`clumsy_crucible`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    solve::<1, 3>(input)
}

/// Solver for part 2 of 2023 Day 17 (`clumsy_crucible`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    solve::<4, 10>(input)
}
