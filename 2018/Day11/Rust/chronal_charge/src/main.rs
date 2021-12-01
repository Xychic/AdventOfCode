use std::fs;

type Input = usize;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    println!("Part 1: {}", part_1(input));
    println!("Part 2: {}", part_2(input));
}

fn parse(input: &str) -> Input {
    input.trim().parse().unwrap()
}

fn part_1(input: Input) -> String {
    let mut grid = vec![vec![0; 300]; 300];
    for (x, row) in grid.iter_mut().enumerate() {
        for (y, pos) in row.iter_mut().enumerate() {
            *pos = score(x, y, input);
        }
    }

    let mut best_score = isize::MIN;
    let mut best = (0, 0);

    for x in 0..289 {
        for y in 0..298 {
            let mut total_score = 0;
            for dx in 0..=2 {
                for dy in 0..=2 {
                    total_score += grid[x + dx][y + dy];
                }
            }
            if total_score > best_score {
                best_score = total_score;
                best = (x, y);
            }
        }
    }

    format!("{},{}", best.0, best.1)
}

fn part_2(input: Input) -> String {
    let mut grid = vec![vec![0; 300]; 300];
    for (x, row) in grid.iter_mut().enumerate() {
        for (y, pos) in row.iter_mut().enumerate() {
            *pos = score(x, y, input);
        }
    }

    let mut partial_sums = vec![vec![0; 300]; 300];
    for x in 0..300 {
        for y in 0..300 {
            let ps = if x > 0 { partial_sums[x - 1][y] } else { 0 }
                + if y > 0 { partial_sums[x][y - 1] } else { 0 }
                - if x > 0 && y > 0 {
                    partial_sums[x - 1][y - 1]
                } else {
                    0
                }
                + grid[x][y];
            partial_sums[x][y] = ps;
        }
    }

    let mut best_score = isize::MIN;
    let mut best = (0, 0, 0);

    for size in 1..=300 {
        for x in 0..(300 - size + 1) {
            for y in 0..(300 - size + 1) {
                if x >= size - 1 && y >= size - 1 {
                    let score = partial_sums[x][y]
                        - if x > size {
                            partial_sums[x - size][y]
                        } else {
                            0
                        }
                        - if y > size {
                            partial_sums[x][y - size]
                        } else {
                            0
                        }
                        + if x > size && y > size {
                            partial_sums[x - size][y - size]
                        } else {
                            0
                        };
                    if score > best_score {
                        best_score = score;
                        best = (x + 1 - size, y + 1 - size, size);
                    }
                }
            }
        }
    }
    format!("{},{},{}", best.0, best.1, best.2)
}

fn score(x: usize, y: usize, serial: usize) -> isize {
    let rack_id = x + 10;
    let mut power_level = y * rack_id;
    power_level += serial;
    power_level *= rack_id;
    power_level /= 100;
    power_level %= 10;
    power_level as isize - 5
}
