use std::{collections::HashMap, fs};

#[derive(Debug)]
struct Entry {
    num: usize,
    pos: (usize, usize),
    dim: (usize, usize),
}

impl Entry {
    fn from_line(line: &str) -> Entry {
        let line = line.replace(":", "").replace("@ ", "").replace("#", "");
        let vals: Vec<_> = line.split(' ').collect();
        let num = vals[0].parse().unwrap();
        let pos: Vec<_> = vals[1].split(',').map(|x| x.parse().unwrap()).collect();
        let pos = (pos[0], pos[1]);
        let dim: Vec<_> = vals[2].split('x').map(|x| x.parse().unwrap()).collect();
        let dim = (dim[0], dim[1]);
        Entry { num, pos, dim }
    }
}

type Input = Vec<Entry>;

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(input.trim());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn parse(input: &str) -> Input {
    input.split('\n').map(Entry::from_line).collect()
}

fn part_1(input: &Input) -> usize {
    let mut map = HashMap::with_capacity(input.len());
    for entry in input {
        for x in 0..entry.dim.0 {
            for y in 0..entry.dim.1 {
                *map.entry((x + entry.pos.0, y + entry.pos.1)).or_insert(0) += 1;
            }
        }
    }
    map.values().filter(|&&x| x >= 2).count()
}

fn part_2(input: &Input) -> usize {
    let mut map = HashMap::with_capacity(input.len());
    for entry in input {
        for x in 0..entry.dim.0 {
            for y in 0..entry.dim.1 {
                *map.entry((x + entry.pos.0, y + entry.pos.1)).or_insert(0) += 1;
            }
        }
    }
    for entry in input {
        let mut valid = true;
        for x in 0..entry.dim.0 {
            for y in 0..entry.dim.1 {
                if map.get(&(x + entry.pos.0, y + entry.pos.1)).unwrap() >= &2 {
                    valid = false;
                }
            }
            if !valid {
                continue;
            }
        }
        if valid {
            return entry.num;
        }
    }

    unreachable!()
}
