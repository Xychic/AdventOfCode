use std::{collections::HashSet, fs, time::Instant};

type Input<'a> = usize;

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
    input.trim().parse().unwrap()
}

fn part_1(input: &Input) -> usize {
    let house_count = input / 10;
    let mut houses = vec![0; house_count];

    for i in 1..house_count {
        for j in (i..house_count).step_by(i) {
            houses[j] += i * 10;
        }
    }
    for (index, h) in houses.iter().enumerate() {
        if h >= input {
            return index;
        }
    }
    unreachable!()
}

fn part_2(input: &Input) -> usize {
    let house_count = input / 11;
    let mut houses = vec![0; house_count];

    for i in 1..house_count {
        for j in (i..house_count.min(i * 50)).step_by(i) {
            houses[j] += i * 11;
        }
    }
    for (index, h) in houses.iter().enumerate() {
        if h >= input {
            return index;
        }
    }
    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "150";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 8);
    }

    // #[test]
    // fn test_part_2() {
    //     assert_eq!(part_2(&parse(&TEST_INPUT_2)), 0);
    // }
}
