use std::collections::{HashSet, VecDeque};

type Input<'a> = Vec<Vec<char>>;

/// Parser for 2024 Day 12 (`garden_groups`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input.trim().lines().map(|l| l.chars().collect()).collect()
}

/// Solver for part 1 of 2024 Day 12 (`garden_groups`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut seen = HashSet::new();
    let mut ans = 0;
    for (y, line) in input.iter().enumerate() {
        for (x, c) in line.iter().enumerate() {
            if seen.contains(&(x, y)) {
                continue;
            }
            let mut area = 0;
            let mut perimeter = 0;
            let mut queue = VecDeque::new();
            queue.push_back((x, y));
            while let Some(curr @ (curr_x, curr_y)) = queue.pop_front() {
                if seen.contains(&curr) {
                    continue;
                }
                seen.insert(curr);
                area += 1;
                for new @ (new_x, new_y) in [
                    (curr_x, curr_y - 1),
                    (curr_x + 1, curr_y),
                    (curr_x, curr_y + 1),
                    (curr_x - 1, curr_y),
                ] {
                    if new_x < line.len() && new_y < input.len() && input[new_y][new_x] == *c {
                        queue.push_back(new);
                    } else {
                        perimeter += 1;
                    }
                }
            }
            ans += area * perimeter;
        }
    }
    ans
}

/// Solver for part 2 of 2024 Day 12 (`garden_groups`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut seen = HashSet::new();
    let mut ans = 0;
    for (y, line) in input.iter().enumerate() {
        for (x, c) in line.iter().enumerate() {
            if seen.contains(&(x, y)) {
                continue;
            }
            let mut area = 0;
            let mut corners = 0;
            let mut queue = VecDeque::new();
            queue.push_back((x, y));
            while let Some(curr @ (curr_x, curr_y)) = queue.pop_front() {
                if seen.contains(&curr) {
                    continue;
                }
                seen.insert(curr);
                area += 1;
                corners += get_corners(curr_x, curr_y, input);

                for new @ (new_x, new_y) in [
                    (curr_x, curr_y - 1),
                    (curr_x + 1, curr_y),
                    (curr_x, curr_y + 1),
                    (curr_x - 1, curr_y),
                ] {
                    if new_x < line.len() && new_y < input.len() && input[new_y][new_x] == *c {
                        queue.push_back(new);
                    }
                }
            }
            ans += area * corners;
        }
    }
    ans
}

fn get_corners(curr_x: usize, curr_y: usize, input: &Input) -> usize {
    let mut corners = 0;
    for (i, &(new_x, new_y)) in [
        (curr_x, curr_y - 1),
        (curr_x + 1, curr_y),
        (curr_x, curr_y + 1),
        (curr_x - 1, curr_y),
    ]
    .iter()
    .enumerate()
    {
        if new_y < input.len()
            && new_x < input[new_y].len()
            && input[new_y][new_x] == input[curr_y][curr_x]
        {
            continue;
        }
        let (corner_x, corner_y) = [
            (curr_x + 1, curr_y),
            (curr_x, curr_y + 1),
            (curr_x - 1, curr_y),
            (curr_x, curr_y - 1),
        ][i];
        if corner_x >= input[curr_y].len()
            || corner_y >= input.len()
            || input[corner_y][corner_x] != input[curr_y][curr_x]
        {
            corners += 1;
            continue;
        }
        let (corner_x, corner_y) = [
            (new_x + 1, new_y),
            (new_x, new_y + 1),
            (new_x - 1, new_y),
            (new_x, new_y - 1),
        ][i];
        if corner_x < input[curr_y].len()
            && corner_y < input.len()
            && input[corner_y][corner_x] == input[curr_y][curr_x]
        {
            corners += 1;
        }
    }
    corners
}
