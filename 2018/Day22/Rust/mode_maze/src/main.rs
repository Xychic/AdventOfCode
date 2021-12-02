use regex::Regex;
use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap, HashSet},
    fs,
    time::Instant,
};

#[derive(Clone, Eq, PartialEq, Debug)]
struct State {
    cost: usize,
    position: (usize, usize),
    item: usize,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.position.cmp(&other.position))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

type Input = (usize, (usize, usize));

fn main() {
    let raw_input = "depth: 510
target: 10,10";
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    let start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1(input),
        Instant::now() - start
    );
    let start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2(input),
        Instant::now() - start
    );
}

fn parse(input: &str) -> Input {
    let re = Regex::new(r"depth: ([0-9]+)\ntarget: ([0-9]+),([0-9]+)").unwrap();
    let caps = re.captures(input.trim()).unwrap();
    (
        caps[1].parse().unwrap(),
        (caps[2].parse().unwrap(), caps[3].parse().unwrap()),
    )
}

fn part_1((depth, target): Input) -> usize {
    create_map(&(depth, target), target)
        .iter()
        .map(|x| x.iter().sum::<usize>())
        .sum()
}

fn part_2((depth, target): Input) -> usize {
    let mut heap: BinaryHeap<State> = BinaryHeap::new();
    let mut seen = HashSet::new();
    let map = create_map(&(depth, target), (target.0 + target.1, target.1 + target.0));

    heap.push(State {
        cost: 0,
        position: (0, 0),
        item: 1,
    });

    let dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)];

    while let Some(State {
        cost,
        position: (x, y),
        item,
    }) = heap.pop()
    {
        if (x, y) == target && item == 1 {
            return cost;
        }

        if seen.contains(&((x, y), item)) {
            continue;
        }
        seen.insert(((x, y), item));

        for i in 0..=2 {
            if i == map[y][x] || i == item {
                continue;
            }
            heap.push(State {
                cost: cost + 7,
                position: (x, y),
                item: i,
            });
        }

        for (dx, dy) in dirs {
            let new_x = dx + x as isize;
            let new_y = dy + y as isize;
            if new_x < 0
                || new_y < 0
                || map[0].len() <= new_x as usize
                || map.len() <= new_y as usize
            {
                continue;
            }
            let new_x = new_x as usize;
            let new_y = new_y as usize;

            if map[new_y][new_x] != item {
                heap.push(State {
                    cost: cost + 1,
                    position: (new_x, new_y),
                    item,
                });
            }
        }
    }

    unreachable!()
}

fn create_map((depth, (t_x, t_y)): &Input, (x_size, y_size): (usize, usize)) -> Vec<Vec<usize>> {
    let mut map = vec![vec![0; x_size + 1]; y_size + 1];
    for x in 0..=x_size {
        for y in 0..=y_size {
            if y == 0 {
                map[y][x] = (x * 16807 + depth) % 20183;
            } else if x == 0 {
                map[y][x] = (y * 48271 + depth) % 20183;
            } else {
                map[y][x] = (map[y - 1][x] * map[y][x - 1] + depth) % 20183;
            }
        }
    }
    map[*t_y][*t_x] = *depth;
    for row in map.iter_mut() {
        for region in row.iter_mut() {
            *region %= 3;
        }
    }

    map
}

fn _display(map: &Vec<Vec<usize>>, (t_x, t_y): &(usize, usize)) {
    for (y, row) in map.iter().enumerate() {
        for (x, region) in row.iter().enumerate() {
            if (x, y) == (0, 0) {
                print!("M");
            } else if (x, y) == (*t_x, *t_y) {
                print!("T");
            } else {
                print!(
                    "{}",
                    match region {
                        0 => ".",
                        1 => "=",
                        2 => "|",
                        _ => unreachable!(),
                    }
                );
            }
        }
        println!();
    }
}
