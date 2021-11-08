use std::{cmp::Ordering, fs};

use itertools::Itertools;

type FloorState = (usize, Vec<Vec<isize>>);

#[derive(Clone, Eq, PartialEq, Debug)]
struct State {
    cost: usize,
    state: FloorState,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
//     let input = "The first floor contains a strontium-compatible microchip, and a plutonium-compatible microchip.
// The second floor contains a strontium generator.
// The third floor contains a plutonium generator.
// The fourth floor contains nothing relevant.";
    let input: Vec<_> = input.trim().split('\n').map(|x| parse_line(x)).collect();

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn part_1(lines: &[Vec<isize>]) -> usize {
    let mut floors: Vec<_> = lines.iter().map(|x| x.len()).collect();
    let mut total = 0;
    for floor in 0..floors.len()-1 {
        total += 2 * floors[floor] - 3;
        floors[floor+1] += floors[floor];
    }

    total
}

fn part_2(lines: &[Vec<isize>]) -> usize {
    let mut floors: Vec<_> = lines.iter().map(|x| x.len()).collect();
    let mut total = 0;
    floors[0] += 4;
    for floor in 0..floors.len()-1 {
        total += 2 * floors[floor] - 3;
        floors[floor+1] += floors[floor];
    }

    total
}

fn parse_line(line: &str) -> Vec<isize> {
    let res = Vec::new();
    if line.contains("nothing relevant") { return res; }
    let line = line.replace(" and a", " a").replace(".", "");
    let line = line.split(' ').skip(5).collect::<Vec<_>>().join(" ");
    let mut line: Vec<_> = line.split(", a ").map(|x| {
        match x {
            "strontium generator" => -1,
            "strontium-compatible microchip" => 1,
            "plutonium generator" => -2,
            "plutonium-compatible microchip" => 2,
            "thulium generator" => -3,
            "thulium-compatible microchip" => 3,
            "ruthenium generator" => -4,
            "ruthenium-compatible microchip" => 4,
            "curium generator" => -5,
            "curium-compatible microchip" => 5,
            _ => panic!("Unknown item \"{}\"", x)
        }
    }).collect();
    line.sort();
    line
}

fn _is_safe(floor: &[isize]) -> bool {
    floor.iter().min().unwrap_or(&1) > &0 ||
    floor.iter().all(|&x| x < 0 || floor.contains(&-x))
}

fn _get_next_states(state: &FloorState) -> Vec<FloorState> {
    let mut states = Vec::new();
    let (floor, floors) = state;
    let mut moves: Vec<_> = floors[*floor].iter().combinations(2).unique().collect();
    moves.extend(floors[*floor].iter().map(|x| vec![x]));

    let mut directions = Vec::new();
    if floor < &floors.len() { directions.push(1); }
    if floor != &0 && !floors[floor-1].is_empty() { directions.push(-1); }

    for m in moves {
        for d in &directions {
            let mut new_floors = floors.clone();
            let new_floor = (*floor as isize + *d) as usize;
            for &item in &m {
                new_floors[new_floor].push(*item);
                let index = new_floors[*floor].iter().position(|x| *x == *item).unwrap();
                new_floors[*floor].remove(index);
            }
            if ! _is_safe(&new_floors[*floor]) || ! _is_safe(&new_floors[new_floor]) {
                continue;
            }
            new_floors[new_floor].sort();
            new_floors[*floor].sort();
            states.push((new_floor, new_floors));
        }
    }

    states
}
