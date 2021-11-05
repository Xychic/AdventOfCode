use std::fs;
use std::cmp::Ordering;
use std::collections::{HashMap, BinaryHeap};

type Node = (usize, usize);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
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
    let input: String = fs::read_to_string("../../../input.txt").expect("error reading file");
    let num: usize = input.trim().parse::<usize>().unwrap();
    
    println!("Part 1: {}", part_1(num));
    println!("Part 2: {}", part_2(num));
}

fn part_1(num: usize) -> usize {
    let mut distances: HashMap<Node, usize> = HashMap::new();
    let mut heap: BinaryHeap<State> = BinaryHeap::new();
    let mut path: HashMap<Node, Node> = HashMap::new();
    
    let start = (1, 1);
    let end = (31, 39);

    distances.insert(start, 0);
    heap.push(State {cost: 0, position: start});

    while let Some(State {cost, position}) = heap.pop() {
        if position == end { 
            return cost; 
        }

        if &cost > distances.get(&position).unwrap_or(&usize::MAX) { continue; }

        for edge in get_neighbours(position, num) {
            let next = State{cost: cost + 1, position: edge};

            if &next.cost < distances.get(&edge).unwrap_or(&usize::MAX) {
                path.insert(next.position, position);
                heap.push(next);
                distances.insert(next.position, next.cost);
            }
        }
    }
    0
}

fn part_2(num: usize) -> usize {
    let mut distances: HashMap<Node, usize> = HashMap::new();
    let mut heap: BinaryHeap<State> = BinaryHeap::new();
    let mut path: HashMap<Node, Node> = HashMap::new();
    
    let start = (1, 1);

    distances.insert(start, 0);
    heap.push(State {cost: 0, position: start});

    while let Some(State {cost, position}) = heap.pop() {
        if cost > 50 {
            break;
        }

        if &cost > distances.get(&position).unwrap_or(&usize::MAX) { continue; }

        for edge in get_neighbours(position, num) {
            let next = State{cost: cost + 1, position: edge};

            if &next.cost < distances.get(&edge).unwrap_or(&usize::MAX) {
                path.insert(next.position, position);
                heap.push(next);
                distances.insert(next.position, next.cost);
            }
        }
    }

    count_less_than_50(&distances)
}

fn is_wall(x: usize, y: usize, num: usize) -> bool {
    let a: usize = x*x + 3*x + 2*x*y + y + y*y + num;
    format!("{:b}", a)          // Format as binary string
        .chars()                // Get the chars
        .filter(|a| a == &'1')  // Filter '1's out
        .count()                // Count them
        % 2 == 1                // See if there is an odd amount
}

fn get_neighbours((x, y): Node, num: usize) -> Vec<Node>{
    let mut neighbours: Vec<Node> = Vec::new();
    let dirs: Vec<(isize, isize)> = vec![(-1, 0), (1, 0), (0, -1), (0, 1)];

    for (dx, dy) in dirs {
        let temp_x = x as isize + dx;
        let temp_y = y as isize + dy;

        if temp_x.is_negative() || temp_y.is_negative() {
            continue;
        }
        if ! is_wall(temp_x as usize, temp_y as usize, num) {
            neighbours.push((temp_x as usize, temp_y as usize));
        }

    }
    neighbours
}

fn count_less_than_50(distances: &HashMap<Node, usize>) -> usize {
    let mut count = 0;
    for (_, v) in distances {
        if v <= &50 {
            count += 1;
        }
    }
    count
}
