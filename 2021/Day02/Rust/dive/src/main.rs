use std::{fs, time::Instant};

use regex::Regex;

type Input = Vec<(String, usize)>;

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
    let re = Regex::new(r"(forward|up|down) ([0-9]+)").unwrap();
    input
        .trim()
        .split('\n')
        .map(|line| {
            let caps = re.captures(line).unwrap();
            (caps[1].to_string(), caps[2].parse().unwrap())
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    let mut pos = 0;
    let mut depth = 0;
    for (action, amount) in input {
        match action.as_str() {
            "forward" => pos += amount,
            "up" => depth -= amount,
            "down" => depth += amount,
            _ => unreachable!(),
        }
    }
    pos * depth
}

fn part_2(input: &Input) -> usize {
    let mut pos = 0;
    let mut depth = 0;
    let mut aim = 0;
    for (action, amount) in input {
        match action.as_str() {
            "forward" => {
                pos += amount;
                depth += aim * amount
            }
            "up" => aim -= amount,
            "down" => aim += amount,
            _ => unreachable!(),
        }
    }
    pos * depth
}
