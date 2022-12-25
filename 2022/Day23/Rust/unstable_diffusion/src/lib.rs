use std::collections::{HashMap, HashSet};

type Point = (isize, isize);
type Input = HashSet<Point>;

const N: isize = -1;
const S: isize = 1;
const E: isize = 1;
const W: isize = -1;

const SPACES: [(Point, [Point; 3]); 4] = [
    ((0, N), [(0, N), (W, N), (E, N)]),
    ((0, S), [(0, S), (W, S), (E, S)]),
    ((W, 0), [(W, 0), (W, N), (W, S)]),
    ((E, 0), [(E, 0), (E, N), (E, S)]),
];

pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars().enumerate().map(move |(x, c)| match c {
                '#' => (x as isize, y as isize),
                _ => (-1, -1),
            })
        })
        .filter(|&x| x != (-1, -1))
        .collect()
}

#[allow(dead_code)]
fn show(elves: &Input) {
    let mut max_x = isize::MIN;
    let mut min_x = isize::MAX;
    let mut max_y = isize::MIN;
    let mut min_y = isize::MAX;

    for &(x, y) in elves {
        max_x = max_x.max(x);
        min_x = min_x.min(x);
        max_y = max_y.max(y);
        min_y = min_y.min(y);
    }

    let input_set: HashSet<_> = elves.iter().collect();
    for y in min_y..=max_y {
        let mut line = String::with_capacity((max_y - min_y) as usize);
        for x in min_x..=max_x {
            line += if input_set.contains(&(x, y)) {
                "#"
            } else {
                "."
            }
        }
        println!("{line}");
    }
    println!();
}

fn step(elves: &mut Input, dir_index: usize) -> bool {
    let mut chosen = HashMap::new();
    for &(x, y) in &*elves {
        let mut has_neighbour = false;
        'outer: for dx in -1..2 {
            for dy in -1..2 {
                if dx == 0 && dy == 0 {
                    continue;
                }
                if elves.contains(&(x + dx, y + dy)) {
                    has_neighbour = true;
                    break 'outer;
                }
            }
        }
        if !has_neighbour {
            continue;
        }

        for i in 0..4 {
            let index = (dir_index + i) % 4;
            if SPACES[index]
                .1
                .iter()
                .all(|(dx, dy)| !elves.contains(&(x + dx, y + dy)))
            {
                let (dx, dy) = SPACES[index].0;
                let new = (x + dx, y + dy);
                let who_chose = chosen.entry(new).or_insert(Vec::with_capacity(1));
                who_chose.push((x, y));

                break;
            }
        }
    }

    let mut changed = false;
    for (new, who_chose) in chosen {
        if who_chose.len() == 1 {
            elves.remove(&who_chose[0]);
            elves.insert(new);
            changed = true;
        }
    }
    changed
}

pub fn part_1(input: &Input) -> usize {
    let mut elves = input.to_owned();

    for dir_index in 0..10 {
        step(&mut elves, dir_index);
    }

    let mut max_x = isize::MIN;
    let mut min_x = isize::MAX;
    let mut max_y = isize::MIN;
    let mut min_y = isize::MAX;

    for &(x, y) in &elves {
        max_x = max_x.max(x + 1);
        min_x = min_x.min(x);
        max_y = max_y.max(y + 1);
        min_y = min_y.min(y);
    }
    max_x.abs_diff(min_x) * max_y.abs_diff(min_y) - elves.len()
}

pub fn part_2(input: &Input) -> usize {
    let mut elves = input.to_owned();

    for dir_index in 0.. {
        if !step(&mut elves, dir_index) {
            return dir_index + 1;
        }
    }
    unreachable!()
}
