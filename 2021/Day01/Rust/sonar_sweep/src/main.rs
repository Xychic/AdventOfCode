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
    (0..input.len() - 1)
        .into_iter()
        .filter(|&i| input[i] < input[i + 1])
        .count()
}

fn part_2(input: &Input) -> usize {
    (0..input.len() - 3)
        .into_iter()
        .filter(|&i| input[i] < input[i + 3])
        .count()
}
