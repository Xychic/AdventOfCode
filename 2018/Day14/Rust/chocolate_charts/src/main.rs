use std::{collections::VecDeque, fs};

type Input = usize;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    println!("Part 1: {}", part_1(input));
    println!("Part 2: {}", part_2(input));
}

fn parse(input: &str) -> Input {
    input.trim().parse().unwrap()
}

fn part_1(input: Input) -> String {
    let mut recipes = vec![3, 7];
    let mut elves = vec![0, 1];
    while recipes.len() <= (input + 10) {
        let ans: usize = elves.iter().map(|&i| recipes[i]).sum();
        for c in ans.to_string().chars() {
            recipes.push(c.to_digit(10).unwrap() as usize);
        }
        elves = elves
            .iter()
            .map(|&i| (i + recipes[i] + 1) % recipes.len())
            .collect();
    }
    recipes
        .iter()
        .skip(input)
        .take(10)
        .map(|d| d.to_string())
        .collect::<String>()
}

fn part_2(input: Input) -> usize {
    let mut recipes = vec![3, 7];
    let mut elves = vec![0, 1];
    let target: Vec<_> = input
        .to_string()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as usize)
        .collect();
    let mut current = VecDeque::with_capacity(target.len());
    for index in 1.. {
        while recipes.len() <= (index + 10) {
            let ans: usize = elves.iter().map(|&i| recipes[i]).sum();
            for c in ans.to_string().chars() {
                recipes.push(c.to_digit(10).unwrap() as usize);
            }
            elves = elves
                .iter()
                .map(|&i| (i + recipes[i] + 1) % recipes.len())
                .collect();
        }
        current.pop_front();
        for i in current.len()..target.len() {
            current.push_back(recipes[index + i]);
        }
        if current == target {
            return index;
        }
    }
    unreachable!()
}
