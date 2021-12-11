use std::{fs, time::Instant};

type Input = [[isize; 10]; 10];

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    let start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1(&input),
        Instant::now() - start
    );
    let start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2(&input),
        Instant::now() - start
    );
}

fn parse(input: &str) -> Input {
    let x: Vec<Vec<_>> = input
        .trim()
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap() as isize)
                .collect()
        })
        .collect();
    let mut res = [[0; 10]; 10];
    for (y, row) in x.iter().enumerate() {
        for (x, val) in row.iter().enumerate() {
            res[y][x] = *val;
        }
    }
    res
}

fn part_1(input: &Input) -> usize {
    let mut octo = input.to_owned();
    let mut ans = 0;
    for _ in 0..100 {
        for row in octo.iter_mut() {
            for val in row.iter_mut() {
                *val += 1;
            }
        }

        for y in 0..octo.len() {
            for x in 0..octo[0].len() {
                if octo[y][x] > 9 {
                    ans += flash_rec(x, y, &mut octo);
                }
            }
        }

        for row in octo.iter_mut() {
            for val in row.iter_mut() {
                if *val < 0 {
                    *val = 0;
                }
            }
        }
    }

    ans
}

fn part_2(input: &Input) -> usize {
    let mut octo = input.to_owned();
    for day in 1.. {
        for row in octo.iter_mut() {
            for val in row.iter_mut() {
                *val += 1;
            }
        }

        for y in 0..octo.len() {
            for x in 0..octo[0].len() {
                if octo[y][x] > 9 {
                    flash_rec(x, y, &mut octo);
                }
            }
        }

        let mut ans = true;
        for row in octo.iter_mut() {
            for val in row.iter_mut() {
                if *val < 0 {
                    *val = 0;
                } else {
                    ans = false;
                }
            }
        }
        if ans {
            return day;
        }
    }
    unreachable!()
}

fn flash_rec(x: usize, y: usize, grid: &mut Input) -> usize {
    let mut flashes = 1;
    grid[y][x] = -1;

    for dy in -1..=1 {
        for dx in -1..=1 {
            let new_x = (x as isize + dx) as usize;
            let new_y = (y as isize + dy) as usize;
            if new_x < 10 && new_y < 10 && grid[new_y][new_x] != -1 {
                grid[new_y][new_x] += 1;
                if grid[new_y][new_x] > 9 {
                    flashes += flash_rec(new_x, new_y, grid);
                }
            }
        }
    }
    flashes
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526";

    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 1656);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 195);
    }
}
