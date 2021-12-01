use std::fs;

type Input = Vec<usize>;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
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
