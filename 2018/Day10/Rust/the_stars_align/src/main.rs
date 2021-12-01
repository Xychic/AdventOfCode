use std::{cmp, collections::HashSet, fs};

use itertools::iproduct;

#[derive(Debug, Clone)]
struct Point {
    pos: (isize, isize),
    vel: (isize, isize),
}

impl Point {
    fn step(&mut self) {
        self.pos = (self.pos.0 + self.vel.0, self.pos.1 + self.vel.1);
    }
}

type Input = Vec<Point>;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    println!("Part 1: {}", part_1(&raw_input));
    println!("Part 2: {}", part_2(&raw_input));
}

fn parse(input: &str) -> Input {
    input
        .trim()
        .split('\n')
        .map(|x| {
            let a: Vec<_> = x
                .split("> ")
                .map(|b| {
                    let b: Vec<_> = b
                        .chars()
                        .skip_while(|n| n != &'<')
                        .skip(1)
                        .take_while(|n| n != &'>')
                        .collect::<String>()
                        .split(",")
                        .map(|n| n.trim().parse::<isize>().unwrap())
                        .collect();
                    (b[0], b[1])
                })
                .collect();
            Point {
                pos: a[0],
                vel: a[1],
            }
        })
        .collect()
}

fn part_1(input: &str) -> String {
    let mut input = parse(input);
    while !is_message(&input) {
        for p in input.iter_mut() {
            p.step();
        }
    }
    get_image(&input)
}

fn part_2(input: &str) -> usize {
    let mut input = parse(input);

    let mut time = 0;
    while !is_message(&input) {
        for p in input.iter_mut() {
            p.step();
        }
        time += 1;
    }
    time
}

fn get_image(points: &Input) -> String {
    let mut max_x = 0;
    let mut min_x = isize::MAX;
    let mut max_y = 0;
    let mut min_y = isize::MAX;
    let mut stars = HashSet::with_capacity(points.len());

    for p in points {
        max_x = cmp::max(max_x, p.pos.0);
        min_x = cmp::min(min_x, p.pos.0);
        max_y = cmp::max(max_y, p.pos.1);
        min_y = cmp::min(min_y, p.pos.1);
        stars.insert(p.pos);
    }

    let mut ans = String::new();
    for y in min_y..=max_y {
        ans += "\n";
        for x in min_x..=max_x {
            ans += if stars.contains(&(x, y)) { "#" } else { "." };
        }
    }

    ans
}

fn is_message(points: &Input) -> bool {
    let stars: HashSet<_> = points.iter().map(|p| p.pos).collect();

    let dirs: Vec<_> = iproduct!((-1..=1), (-1..=1))
        .filter(|n| n != &(0, 0))
        .collect();

    for p in points {
        if !dirs
            .iter()
            .any(|(dx, dy)| stars.contains(&(p.pos.0 + dx, p.pos.1 + dy)))
        {
            return false;
        }
    }

    true
}
