use std::{collections::HashSet, fs};

type Input = Vec<isize>;

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(input.trim());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn parse(input: &str) -> Input {
    input.split('\n').map(|x| x.parse().unwrap()).collect()
}

fn part_1(input: &Input) -> isize {
    input.iter().sum()
}

fn part_2(input: &Input) -> isize {
    let mut seen = HashSet::new();
    let iter = input.iter().cycle().scan(0, |acc, &x| {
        *acc += x;
        Some(*acc)
    });
    for c in iter {
        if seen.contains(&c) {
            return c;
        }
        seen.insert(c);
    }

    unreachable!()
}
