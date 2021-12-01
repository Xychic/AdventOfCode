use std::{collections::VecDeque, fs};

type Input = VecDeque<char>;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(raw_input.trim());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(raw_input.trim()));
}

fn parse(input: &str) -> Input {
    input.chars().collect()
}

fn part_1(input: &Input) -> usize {
    let mut changes = true;
    let mut old = input.to_owned();
    while changes {
        changes = false;
        let mut new = VecDeque::with_capacity(input.len());
        while old.len() > 1 {
            let a = old.pop_front().unwrap();
            let b = old.pop_front().unwrap();
            if reacts(a, b) {
                changes = true;
            } else {
                new.push_back(a);
                old.push_front(b);
            }
        }
        if !old.is_empty() {
            new.push_back(old.pop_front().unwrap());
        }
        old = new;
    }
    old.len()
}

fn part_2(input: &str) -> usize {
    String::from_utf8((b'a'..=b'z').collect())
        .unwrap()
        .chars()
        .map(|x| {
            let x = input.replace(x, "").replace(x.to_ascii_uppercase(), "");
            part_1(&parse(&x))
        })
        .min()
        .unwrap()
}

fn reacts(a: char, b: char) -> bool {
    if a.is_lowercase() == b.is_lowercase() {
        return false;
    }
    a.to_ascii_lowercase() == b.to_ascii_lowercase()
}
