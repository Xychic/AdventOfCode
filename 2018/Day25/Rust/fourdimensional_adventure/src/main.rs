use regex::Regex;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fs,
};

type Input = Vec<Point>;
type Point = [isize; 4];

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");

    println!("Part 1: {}", part_1(&raw_input));
}

fn parse(input: &str) -> Input {
    let re = Regex::new(r"(-?[0-9]+),(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)").unwrap();
    input
        .trim()
        .split('\n')
        .map(|row| {
            let caps = re.captures(row).unwrap();
            [
                caps[1].parse().unwrap(),
                caps[2].parse().unwrap(),
                caps[3].parse().unwrap(),
                caps[4].parse().unwrap(),
            ]
        })
        .collect()
}

fn part_1(input: &str) -> usize {
    let points = parse(input);
    let mut next_to = HashMap::with_capacity(points.len());
    for (i, p1) in points.iter().enumerate() {
        for p2 in points.iter().skip(i + 1) {
            if get_dist(p1, p2) <= 3 {
                next_to
                    .entry(p1)
                    .or_insert(Vec::with_capacity(points.len()))
                    .push(p2);
                next_to
                    .entry(p2)
                    .or_insert(Vec::with_capacity(points.len()))
                    .push(p1);
            }
        }
    }

    let mut constellations = 0;
    let mut seen = HashSet::with_capacity(points.len());
    for p in &points {
        if seen.contains(p) {
            continue;
        }

        let mut to_add = VecDeque::with_capacity(points.len());
        to_add.push_back(p.to_owned());
        while let Some(star) = to_add.pop_front() {
            if let Some(x) = next_to.get(&star) {
                for &near in x {
                    if !seen.contains(near) {
                        to_add.push_back(near.to_owned());
                    }
                }
            }
            seen.insert(star);
        }
        constellations += 1;
    }

    constellations
}

fn get_dist(p1: &Point, p2: &Point) -> usize {
    p1.iter()
        .zip(p2.iter())
        .map(|(a, b)| (a - b).abs() as usize)
        .sum()
}
