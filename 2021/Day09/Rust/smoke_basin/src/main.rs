use std::{collections::VecDeque, fs, time::Instant};

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
    let mut lows = Vec::new();
    let x_size = input[0].len();
    let y_size = input.len();
    for (y, row) in input.iter().enumerate() {
        for (x, val) in row.iter().enumerate() {
            if [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
                .iter()
                .map(|&(x2, y2)| {
                    if x2 < x_size && y2 < y_size {
                        input[y2][x2]
                    } else {
                        u32::MAX
                    }
                })
                .all(|x| val < &x)
            {
                lows.push((x, y));
            }
        }
    }

    let mut basins = Vec::with_capacity(lows.len());
    let mut seen = Vec::with_capacity(input.len() * input[0].len());
    for l in lows {
        let mut basin = 0;
        let mut queue = VecDeque::new();
        queue.push_back(l);
        seen.push(l);
        while let Some((x, y)) = queue.pop_front() {
            basin += 1;
            for (x2, y2) in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] {
                if x2 < x_size && y2 < y_size {
                    if !seen.contains(&(x2, y2)) && input[y2][x2] != 9 {
                        queue.push_back((x2, y2));
                        seen.push((x2, y2));
                    }
                }
            }
        }
        basins.push(basin);
    }
    basins.sort_unstable();
    basins.iter().rev().take(3).product()
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
