use std::{collections::HashSet, fs, time::Instant};

type Input = (Vec<bool>, HashSet<(isize, isize)>);

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
    let mut parts = input.trim().split("\n\n");
    let enhance = parts.next().unwrap().chars().map(|c| c=='#').collect();
    let mut grid = HashSet::with_capacity(input.len() - 512);
    for (y, line) in parts.next().unwrap().lines().enumerate() {
        for (x, c) in line.char_indices() {
            if c == '#' {
                grid.insert((x as isize, y as isize));
            }
        }
    }
    (enhance, grid)
}

fn part_1(input: &Input) -> usize {
    do_the_thing(input, 2)
}

fn part_2(input: &Input) -> usize {
    do_the_thing(input, 50)
}

fn do_the_thing((enhance, grid): &Input, steps: usize) -> usize {
    let mut grid = grid.to_owned();

    let mut to_check = HashSet::with_capacity(grid.len() * 2);
    for (x, y) in &grid {
        for dy in -1..=1 {
            for dx in -1..=1 {
                to_check.insert((x + dx, y + dy));
            }
        }
    }

    for i in 1..=steps {
        let mut bin_key = ["0", "1"];
        let mut to_save = true;
        if *enhance.first().unwrap() && !enhance.last().unwrap() {
            if i > 0 && i % 2 == 0 {
                bin_key = ["1", "0"];
            } else {
                to_save = false;
            }
        }

        let mut next_grid = HashSet::with_capacity(to_check.len());
        let mut next_to_check = HashSet::with_capacity(next_grid.len() * 2);

        for (x, y) in to_check {
            let mut val = String::with_capacity(9);
            for dy in -1..=1 {
                for dx in -1..=1 {
                    val += if grid.contains(&(x + dx, y + dy)) {
                        bin_key[1]
                    } else {
                        bin_key[0]
                    };
                }
            }
            let val = usize::from_str_radix(&val, 2).unwrap();
            if enhance[val] == to_save {
                next_grid.insert((x, y));
                for dy in -1..=1 {
                    for dx in -1..=1 {
                        next_to_check.insert((x + dx, y + dy));
                    }
                }
            }
        }

        grid = next_grid;
        to_check = next_to_check;
    }

    grid.len()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 35);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 3351);
    }
}
