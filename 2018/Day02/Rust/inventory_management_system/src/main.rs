use std::{collections::HashMap, fs};

type Input<'a> = Vec<&'a str>;

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(input.trim());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn parse(input: &str) -> Input {
    input.split('\n').collect()
}

fn part_1(input: &Input) -> usize {
    let mut twos = 0;
    let mut threes = 0;
    for row in input {
        let mut char_count = HashMap::with_capacity(row.len());
        for c in row.chars() {
            *char_count.entry(c).or_insert(0) += 1;
        }
        if char_count.values().any(|v| v == &2) {
            twos += 1;
        }
        if char_count.values().any(|v| v == &3) {
            threes += 1;
        }
    }
    twos * threes
}

fn part_2(input: &Input) -> String {
    for (i, a) in input.iter().take(input.len() - 1).enumerate() {
        for b in input.iter().skip(i + 1) {
            if 1 == a
                .chars()
                .zip(b.chars())
                .map(|(a, b)| a == b)
                .filter(|x| !x)
                .count()
            {
                return a
                    .chars()
                    .zip(b.chars())
                    .filter(|(a, b)| a == b)
                    .map(|(a, _)| a)
                    .collect();
            }
        }
    }
    unreachable!()
}
