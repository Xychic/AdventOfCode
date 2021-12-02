use std::{fs, time::Instant};

type Input = Vec<usize>;

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
        .map(|x| x.parse().unwrap())
        .collect()
}

fn part_1(input: &Input) -> usize {
    input
        .iter()
        .zip(input.iter().skip(1))
        .filter(|(a, b)| a < b)
        .count()
}

fn part_2(input: &Input) -> usize {
    input
        .iter()
        .zip(input.iter().skip(3))
        .filter(|(a, b)| a < b)
        .count()
}
