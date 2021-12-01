use std::{collections::VecDeque, fs};

type Input = (Vec<isize>, Vec<String>);

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn parse(input: &str) -> Input {
    let x: Vec<_> = input.trim().split("\n\n").collect();
    let data: Vec<_> = x[0].split(' ').collect();
    let data: Vec<_> = data[2]
        .char_indices()
        .filter(|(_, c)| c == &'#')
        .map(|(i, _)| i as isize)
        .collect();

    let rules: Vec<_> = x[1]
        .split('\n')
        .filter(|l| l.ends_with('#'))
        .map(|l| {
            let l: Vec<_> = l.split(' ').collect();
            l[0].to_string()
        })
        .collect();

    (data, rules)
}

fn part_1((current, rules): &Input) -> isize {
    let mut current = current.to_owned();
    for _ in 0..20 {
        let mut next = Vec::with_capacity(current.len());
        let min = current.iter().min().unwrap();
        let max = current.iter().max().unwrap();
        for i in min - 2..=max + 2 {
            let mut pattern = String::with_capacity(5);
            for d in -2..=2 {
                pattern += if current.contains(&(i + d)) { "#" } else { "." };
            }
            if rules.contains(&pattern) {
                next.push(i);
            }
        }
        current = next;
    }

    current.iter().sum()
}

fn part_2((current, rules): &Input) -> isize {
    let mut current = current.to_owned();
    let mut diffs = VecDeque::with_capacity(100);
    let mut prev = 0;
    for generation in 1.. {
        let score: isize = current.iter().sum();
        diffs.push_back(score - prev);
        prev = score;
        if diffs.len() > 100 {
            diffs.pop_front();
        }

        let mut next = Vec::with_capacity(current.len());
        let min = current.iter().min().unwrap();
        let max = current.iter().max().unwrap();
        for i in min - 2..=max + 2 {
            let mut pattern = String::with_capacity(5);
            for d in -2..=2 {
                pattern += if current.contains(&(i + d)) { "#" } else { "." };
            }
            if rules.contains(&pattern) {
                next.push(i);
            }
        }
        current = next;
        if diffs.len() == 100 && diffs.iter().all(|x| x == &diffs[0]) {
            return current.iter().sum::<isize>() + (50000000000 - generation) * diffs[0];
        }
    }
    unreachable!()
}
