use std::{fs, time::Instant};

use itertools::Itertools;

type Input<'a> = Vec<Vec<usize>>;

fn main() {
    let start = Instant::now();
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    println!("Parsed input in {:?}", Instant::now() - start);

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
        .split("\n\n")
        .map(|x| x.split('\n').map(|y| y.parse().unwrap()).collect())
        .collect()
}

fn part_1(input: &Input) -> usize {
    input.iter().map(|x| x.iter().sum()).max().unwrap()
}

fn part_2(input: &Input) -> usize {
    input
        .iter()
        .map(|x| x.iter().sum::<usize>())
        .sorted_by(|a, b| b.cmp(a))
        .take(3)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 24000);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 45000);
    }
}
