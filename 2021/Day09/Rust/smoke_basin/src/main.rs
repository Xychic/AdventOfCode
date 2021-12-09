use std::{collections::HashMap, fs, time::Instant};

type Input = Vec<Vec<u32>>;

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
    input
        .trim()
        .split('\n')
        .map(|line| line.chars().map(|x| x.to_digit(10).unwrap()).collect())
        .collect()
}

fn part_1(input: &Input) -> u32 {
    let mut ans = 0;
    let x_size = input[0].len();
    let y_size = input.len();
    for (y, row) in input.iter().enumerate() {
        for (x, val) in row.iter().enumerate() {
            if [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
                .iter()
                .map(|(x2, y2)| {
                    if x2 >= &0 && y2 >= &0 && x2 < &x_size && y2 < &y_size {
                        input[*y2][*x2]
                    } else {
                        u32::MAX
                    }
                })
                .all(|x| val < &x)
            {
                ans += val + 1;
            }
        }
    }
    ans
}

fn part_2(input: &Input) -> usize {
    let mut basins = HashMap::new();
    let x_size = input[0].len();
    let y_size = input.len();

    for (y, row) in input.iter().enumerate() {
        for (x, &val) in row.iter().enumerate() {
            if val != 9 {
                let basin = get_basin((x, y), (x_size, y_size), input);
                *basins.entry(basin).or_insert(0 as usize) += 1;
            }
        }
    }

    let mut vals: Vec<_> = basins.values().into_iter().collect();
    vals.sort_unstable();
    vals.iter().rev().take(3).map(|&x| x).product()
}

fn get_basin((x, y): (usize, usize), (max_x, max_y): (usize, usize), data: &Input) -> usize {
    for (x2, y2) in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] {
        if x2 < max_x && y2 < max_y && data[y2][x2] < data[y][x] {
            return get_basin((x2, y2), (max_x, max_y), data);
        }
    }
    x * max_y + y
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        let test_input = "2199943210
3987894921
9856789892
8767896789
9899965678";
        let test_answer = 15;
        assert_eq!(part_1(&parse(&test_input)), test_answer);
    }

    #[test]
    fn test_part_2() {
        let test_input = "2199943210
3987894921
9856789892
8767896789
9899965678";
        let test_answer = 1134;
        assert_eq!(part_2(&parse(&test_input)), test_answer);
    }
}
