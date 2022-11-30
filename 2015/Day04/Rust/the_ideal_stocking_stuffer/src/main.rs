use std::{fs, time::Instant};

type Input<'a> = &'a str;

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
    input.trim()
}

fn part_1(input: &Input) -> usize {
    for i in 0.. {
        if format!("{:?}", md5::compute(format!("{}{}", input, i))).starts_with("00000") {
            return i;
        }
    }
    unreachable!()
}

fn part_2(input: &Input) -> usize {
    for i in 0.. {
        if format!("{:?}", md5::compute(format!("{}{}", input, i))).starts_with("000000") {
            return i;
        }
    }
    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "abcdef";

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 609043);
    }
}
