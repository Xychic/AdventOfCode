use std::{cmp::{self, Ordering}, collections::BinaryHeap, fs};

type Node = ((usize, usize), String);

#[derive(Clone, Eq, PartialEq, Debug)]
struct State {
    cost: usize,
    position: Node,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
            .then_with(|| self.position.cmp(&other.position))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = input.trim();

    println!("Part 1: {}", part_1(input));
    println!("Part 2: {}", part_2(input));
}

fn part_1(input: &str) -> String {
    let mut heap = BinaryHeap::new();
    let start = (0, 0);
    let end = (3, 3);

    heap.push(State {cost: 0, position: (start, input.to_string())});

    while let Some(State {cost, position: (coords, path)}) = heap.pop() {
        if coords == end { 
            return path[input.len()..].to_string();
        }
        for edge in get_neighbours(&(coords, path)) {
            heap.push(State{ cost: cost + 1, position: edge });
        }
    }
    panic!("No solution for part 1 found!")
}

fn part_2(input: &str) -> usize {
    let mut heap = BinaryHeap::new();
    
    let start = (0, 0);
    let end = (3, 3);
    let mut result = 0;

    heap.push(State {cost: 0, position: (start, input.to_string())});

    while let Some(State {cost, position: (coords, path)}) = heap.pop() {
        if coords == end { 
            result = cmp::max(result, cost);
            continue;
        }

        for edge in get_neighbours(&(coords, path)) {
            heap.push(State{cost: cost + 1, position: edge});
        }
    }
    if result == 0 {panic!("No solution for part 2 found!")} else {result}
}

fn get_doors(status: &str) -> Vec<bool> {
    let mut result = Vec::with_capacity(4);
    let mut status_chars = status.chars();
    let open = "bcdef";
    for _ in 0..4 {
        let a = status_chars.next().unwrap();
        result.push(open.contains(a));
    }
    result
}

fn create_digest(input: &str) -> String {
    let result = md5::compute(input.as_bytes());
    format!("{:x}", result)
}

fn get_neighbours(((x, y), string): &Node) -> Vec<Node>{
    let mut neighbours = Vec::with_capacity(4);
    let dirs = vec![((0, -1), 'U'), ((0, 1), 'D'), ((-1, 0), 'L'), ((1, 0), 'R')];
    let doors = get_doors(&create_digest(string));

    for (door_open, ((dx, dy), step)) in doors.iter().zip(dirs) {
        if ! *door_open { continue; }

        let temp_x = *x as isize + dx;
        let temp_y = *y as isize + dy;

        if temp_x.is_negative() || temp_y.is_negative() || temp_x >= 4 || temp_y >= 4 { continue; }
        neighbours.push(((temp_x as usize, temp_y as usize), format!("{}{}", string, step)));

    }
    neighbours
}
