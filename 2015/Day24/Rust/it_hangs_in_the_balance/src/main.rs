use std::{fs, time::Instant};

use itertools::Itertools;

type Input<'a> = Vec<usize>;

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
        .map(|l| l.parse().unwrap())
        .collect()
}

fn part_1(input: &Input) -> usize {
    let group_size = input.iter().sum::<usize>() / 3;
    let input_size = input.len();

    for index in 1..input_size {
        // dbg!(index);
        if let Some(ans) = input
            .iter()
            .combinations(index)
            .filter(|c| c.iter().map(|x| *x).sum::<usize>() == group_size)
            .map(|c| c.iter().map(|x| *x).product())
            .min()
        {
            return ans;
        }
    }

    unreachable!()
}

fn part_2(input: &Input) -> usize {
    let group_size = input.iter().sum::<usize>() / 4;
    let input_size = input.len();

    for index in 1..input_size {
        // dbg!(index);
        if let Some(ans) = input
            .iter()
            .combinations(index)
            .filter(|c| c.iter().map(|x| *x).sum::<usize>() == group_size)
            .map(|c| c.iter().map(|x| *x).product())
            .min()
        {
            return ans;
        }
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "1
2
3
4
5
7
8
9
10
11";
    // const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 99);
    }

    // #[test]
    // fn test_part_2() {
    //     assert_eq!(part_2(&parse(&TEST_INPUT_2)), 0);
    // }
}
