use std::{cmp::Ordering, collections::BinaryHeap, fs, time::Instant};

use regex::Regex;

#[derive(Debug, PartialEq, Eq)]
struct Nanobot {
    pos: [isize; 3],
    radius: usize,
}

impl Nanobot {
    fn get_dist(&self, pos: &[isize; 3]) -> usize {
        self.pos
            .iter()
            .zip(pos)
            .map(|(a, b)| (a - b).abs() as usize)
            .sum()
    }

    fn in_range(&self, other: &Nanobot) -> bool {
        self.get_dist(&other.pos) <= self.radius
    }
}

#[derive(Debug, Eq, PartialEq)]
struct BoundingBox {
    mins: [isize; 3],
    maxs: [isize; 3],
    size: usize,
    bots: usize,
}

impl BoundingBox {
    fn new(mins: [isize; 3], maxs: [isize; 3], size: usize, bots: &Input) -> BoundingBox {
        let mut bb = BoundingBox {
            mins,
            maxs,
            size,
            bots: 0,
        };
        let bot_count = bots.iter().filter(|bot| bb.intersects(bot)).count();
        bb.bots = bot_count;
        bb
    }

    fn intersects(&self, bot: &Nanobot) -> bool {
        self.get_dist(bot.pos) <= bot.radius
    }

    fn get_dist(&self, pos: [isize; 3]) -> usize {
        (0..=2)
            .map(|i| {
                if pos[i] <= self.mins[i] {
                    (self.mins[i] - pos[i]) as usize
                } else if pos[i] >= self.maxs[i] - 1 {
                    (pos[i] - (self.maxs[i] - 1)) as usize
                } else {
                    0
                }
            })
            .sum()
    }
}

impl Ord for BoundingBox {
    fn cmp(&self, other: &Self) -> Ordering {
        self.bots.cmp(&other.bots).then_with(|| {
            self.size
                .cmp(&other.size)
                .then_with(|| other.get_dist([0, 0, 0]).cmp(&self.get_dist([0, 0, 0])))
        })
    }
}

impl PartialOrd for BoundingBox {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

type Input = Vec<Nanobot>;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    let start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1(&input),
        Instant::now() - start
    );
    let start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2(&input),
        Instant::now() - start
    );
}

fn parse(input: &str) -> Input {
    let re = Regex::new(r"pos=<(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)>, r=([0-9]+)").unwrap();
    input
        .trim()
        .split('\n')
        .map(|line| {
            let caps = re.captures(line).unwrap();
            Nanobot {
                pos: [
                    caps[1].parse().unwrap(),
                    caps[2].parse().unwrap(),
                    caps[3].parse().unwrap(),
                ],
                radius: caps[4].parse().unwrap(),
            }
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    let best = input.iter().max_by_key(|n| n.radius).unwrap();
    input.iter().filter(|&n| best.in_range(n)).count()
}

fn part_2(input: &Input) -> usize {
    let mut max_abs_dist = 0;
    for bot in input {
        for p in bot.pos {
            max_abs_dist = max_abs_dist.max(p.abs() as usize + bot.radius);
        }
    }

    let mut box_dist = 1;
    while box_dist <= max_abs_dist {
        box_dist *= 2;
    }

    let mut heap = BinaryHeap::new();
    heap.push(BoundingBox::new(
        [-(box_dist as isize); 3],
        [box_dist as isize; 3],
        box_dist * 2,
        input,
    ));

    let reductions = [
        [0, 0, 0],
        [0, 0, 1],
        [0, 1, 0],
        [0, 1, 1],
        [1, 0, 0],
        [1, 0, 1],
        [1, 1, 0],
        [1, 1, 1],
    ];

    while let Some(b) = heap.pop() {
        if b.size == 1 {
            return b.get_dist([0, 0, 0]);
        }

        let new_size = b.size / 2;
        for vertex in reductions {
            let new_mins = [
                b.mins[0] + (new_size * vertex[0]) as isize,
                b.mins[1] + (new_size * vertex[1]) as isize,
                b.mins[2] + (new_size * vertex[2]) as isize,
            ];
            let new_maxs = [
                new_mins[0] + new_size as isize,
                new_mins[1] + new_size as isize,
                new_mins[2] + new_size as isize,
            ];
            heap.push(BoundingBox::new(new_mins, new_maxs, new_size, input));
        }
    }
    unreachable!()
}
