use std::collections::{HashSet, VecDeque};

type Input<'a> = Vec<Vec<char>>;

const UP: usize = 0;
const RIGHT: usize = 1;
const DOWN: usize = 2;
const LEFT: usize = 3;

/// Parser for 2023 Day 16 (`the_floor_will_be_lava`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input.trim().lines().map(|l| l.chars().collect()).collect()
}

fn solve(grid: &Input, start: ((usize, usize), usize)) -> usize {
    let mut beams = VecDeque::new();
    let mut seen = HashSet::new();
    beams.push_back(start);

    while let Some(beam @ ((x, y), dir)) = beams.pop_front() {
        seen.insert(beam);

        let dirs = match (grid[y][x], dir) {
            ('.', dir) => [Some(dir), None],
            ('-', LEFT | RIGHT) | ('|', UP | DOWN) => [Some(dir), None],
            ('/', RIGHT) | ('\\', LEFT) => [Some(UP), None],
            ('/', UP) | ('\\', DOWN) => [Some(RIGHT), None],
            ('/', LEFT) | ('\\', RIGHT) => [Some(DOWN), None],
            ('/', DOWN) | ('\\', UP) => [Some(LEFT), None],
            ('|', LEFT | RIGHT) => [Some(UP), Some(DOWN)],
            ('-', UP | DOWN) => [Some(LEFT), Some(RIGHT)],
            _ => unreachable!(),
        };
        for &dir in dirs.iter().flatten() {
            let x = [x, x + 1, x, x - 1][dir];
            let y = [y - 1, y, y + 1, y][dir];
            if x >= grid[0].len() || y >= grid.len() {
                continue;
            }
            if seen.contains(&((x, y), dir)) {
                continue;
            }
            beams.push_back(((x, y), dir));
        }
    }
    seen.iter()
        .map(|&(pos, _)| pos)
        .collect::<HashSet<(usize, usize)>>()
        .len()
}

/// Solver for part 1 of 2023 Day 16 (`the_floor_will_be_lava`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    solve(input, ((0, 0), RIGHT))
}

/// Solver for part 2 of 2023 Day 16 (`the_floor_will_be_lava`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let rows = input.len();
    let cols = input[0].len();
    let mut starts = Vec::with_capacity(2 * (rows + cols));
    for y in 0..rows {
        starts.push(((0, y), RIGHT));
        starts.push(((cols - 1, y), LEFT));
    }
    for x in 0..cols {
        starts.push(((x, 0), DOWN));
        starts.push(((x, rows - 1), UP));
    }

    starts.iter().map(|&s| solve(input, s)).max().unwrap()
}
