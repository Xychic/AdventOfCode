use std::{fs, time::Instant};

type Input<'a> = Vec<((usize, usize), (usize, usize))>;

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
        .split('\n')
        .map(|line| {
            let (elf_1, elf_2) = line.split_once(',').unwrap();
            let (a0, a1) = elf_1.split_once('-').unwrap();
            let (b0, b1) = elf_2.split_once('-').unwrap();
            (
                (a0.parse().unwrap(), a1.parse().unwrap()),
                (b0.parse().unwrap(), b1.parse().unwrap()),
            )
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    input
        .iter()
        .filter(|((a0, a1), (b0, b1))| (a0 <= b0 && b1 <= a1) || (b0 <= a0 && a1 <= b1))
        .count()
}

fn part_2(input: &Input) -> usize {
    input
        .iter()
        .filter(|((a0, a1), (b0, b1))| b0.max(a0) <= a1.min(b1))
        .count()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 2);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 4);
    }
}
