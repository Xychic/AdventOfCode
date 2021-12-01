use std::{collections::HashMap, fs};

use itertools::iproduct;

type Input = Vec<(usize, usize)>;

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(input.trim());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn parse(input: &str) -> Input {
    input
        .split('\n')
        .map(|x| {
            let x: Vec<_> = x.split(", ").map(|c| c.parse().unwrap()).collect();
            (x[0], x[1])
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    let min_x = input.iter().min_by_key(|(a, _)| a).unwrap().0;
    let max_x = input.iter().max_by_key(|(a, _)| a).unwrap().0;
    let min_y = input.iter().min_by_key(|(_, a)| a).unwrap().1;
    let max_y = input.iter().max_by_key(|(_, a)| a).unwrap().1;

    let mut points = HashMap::with_capacity(input.len());
    for x in min_x..max_x {
        for y in min_y..max_y {
            let distances: Vec<_> = input
                .iter()
                .map(|a| (a, manhattan_distance(a, &(x, y))))
                .collect();
            let closest = distances.iter().min_by_key(|(_, x)| x).unwrap();
            if distances.iter().filter(|(_, a)| a == &closest.1).count() == 1 {
                *points.entry(closest.0).or_insert(0) += 1;
            }
        }
    }

    let mut banned = Vec::with_capacity(4);
    for x in [min_x, max_x] {
        for y in [min_y, max_y] {
            banned.push(
                input
                    .iter()
                    .min_by_key(|&a| manhattan_distance(a, &(x, y)))
                    .unwrap(),
            );
        }
    }

    *points
        .iter()
        .filter(|(a, _)| !banned.contains(a))
        .max_by_key(|(_, &a)| a)
        .unwrap()
        .1
}

fn part_2(input: &Input) -> usize {
    let min_x = input.iter().min_by_key(|(a, _)| a).unwrap().0;
    let max_x = input.iter().max_by_key(|(a, _)| a).unwrap().0;
    let min_y = input.iter().min_by_key(|(_, a)| a).unwrap().1;
    let max_y = input.iter().max_by_key(|(_, a)| a).unwrap().1;

    iproduct!((min_x..max_x), (min_y..max_y))
        .filter(|a| {
            input
                .iter()
                .map(|b| manhattan_distance(a, b))
                .sum::<usize>()
                < 10000
        })
        .count()
}

fn manhattan_distance(a: &(usize, usize), b: &(usize, usize)) -> usize {
    ((a.0 as isize - b.0 as isize).abs() + (a.1 as isize - b.1 as isize).abs()) as usize
}
