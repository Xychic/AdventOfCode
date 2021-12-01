use std::{
    collections::{HashMap, VecDeque},
    fs,
};

#[derive(Debug)]
struct Input {
    players: usize,
    marbles: usize,
}

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(input.trim());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn parse(input: &str) -> Input {
    let words: Vec<_> = input.split(' ').collect();
    let players = words[0].parse().unwrap();
    let marbles = words[6].parse().unwrap();
    Input { players, marbles }
}

fn part_1(input: &Input) -> usize {
    let mut circle = VecDeque::with_capacity(input.marbles);
    let mut scores = HashMap::with_capacity(input.players);
    let mut player = 0;
    circle.push_back(0);
    for i in 1..=input.marbles {
        if circle.len() < 2 {
            circle.push_back(i);
        } else if i % 23 == 0 {
            for _ in 0..=6 {
                let tmp = circle.pop_back().unwrap();
                circle.push_front(tmp);
            }
            *scores.entry(player).or_insert(0) += i + circle.pop_back().unwrap();
            let tmp = circle.pop_front().unwrap();
            circle.push_back(tmp);
        } else {
            let tmp = circle.pop_front().unwrap();
            circle.push_back(tmp);
            circle.push_back(i);
        }
        player = (player + 1) % input.players;
    }
    *scores.values().max().unwrap()
}

fn part_2(input: &Input) -> usize {
    part_1(&Input {
        players: input.players,
        marbles: input.marbles * 100,
    })
}
