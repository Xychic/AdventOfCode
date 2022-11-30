use std::{fs, time::Instant};

type Input<'a> = Vec<isize>;

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
        .chars()
        .map(|c| match c {
            '(' => 1,
            ')' => -1,
            _otherwise => unreachable!(),
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    input.iter().sum::<isize>() as usize
}

fn part_2(input: &Input) -> usize {
    let mut floor = 0;
    for (index, step) in input.iter().enumerate() {
        floor += step;
        if floor == -1 {
            return index + 1;
        }
    }
    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "(()(()(";
    const TEST_INPUT_2: &str = "()())";

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 3);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 5);
    }
}
