#![allow(dead_code, unused_variables)]


use std::fs;
use std::hash::Hash;
use std::cmp::Ordering;
use std::cmp;
use std::collections::{HashMap, BinaryHeap};

type Node = (usize, usize);

struct DefaultMap<'a, A, B>
where A: Eq + Hash + Copy,
B : Copy {
    data: HashMap<A, B>,
    default: &'a dyn Fn(A) -> B
}

impl<'a, A: Eq + Hash + Copy, B: Copy> DefaultMap<'a, A, B> {
    fn new(default: &'a dyn Fn(A) -> B) -> Self {
        DefaultMap {
            data: HashMap::new(),
            default: default
        }
    }

    fn get(&mut self, key: A) -> &B {
        if ! self.data.contains_key(&key) {
            self.data.insert(key, (self.default)(key));
        }
        self.data.get(&key).unwrap()
    }

    fn set(&mut self, key: A, value: B) {
        self.data.insert(key, value);
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
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
    println!("{}", num);
    
    println!("Part 1: {}", part_1(num));
    println!("Part 2: {}", part_2(num));
}

fn part_1(num: usize) -> usize {
    let walls_func = |(a, b)| is_wall(a, b, num);
    let mut walls: DefaultMap<Node, bool> = DefaultMap::new(&walls_func);
    
    let distance_func = |_| usize::MAX;
    let mut distances: DefaultMap<Node, usize> = DefaultMap::new(&distance_func);
    let mut heap: BinaryHeap<State> = BinaryHeap::new();
    let mut path: HashMap<Node, Node> = HashMap::new();
    
    let start: Node = (1, 1);
    let end: Node = (31, 39);

    distances.set(start, 0);
    heap.push(State {cost: 0, position: start});

    while let Some(State {cost, position}) = heap.pop() {
        // // Alternatively we could have continued to find all shortest paths
        if position == end { 
            // let mut path_to_end: Vec<Node> = Vec::new();
            // let mut p: Node = *path.get(&end).unwrap();
            // path_to_end.push(p);
            // while p != start {
            //     p = *path.get(&p).unwrap();
            //     path_to_end.push(p);
            // }
            // path_to_end.reverse();
            // draw_board(&mut walls, path_to_end);
            return cost; 
        }

        if &cost > distances.get(position) { continue; }

        for edge in get_neighbours(&mut walls, position) {
            let next = State{cost: cost + 1, position: edge};

            if &next.cost < distances.get(edge) {
                path.insert(next.position, position);
                heap.push(next);
                distances.set(next.position, next.cost)
            }
        }
    }
    0
}

fn part_2(num: usize) -> usize {
    let walls_func = |(a, b)| is_wall(a, b, num);
    let mut walls: DefaultMap<Node, bool> = DefaultMap::new(&walls_func);
    
    let distance_func = |_| usize::MAX;
    let mut distances: DefaultMap<Node, usize> = DefaultMap::new(&distance_func);
    let mut heap: BinaryHeap<State> = BinaryHeap::new();
    let mut path: HashMap<Node, Node> = HashMap::new();
    
    let start: Node = (1, 1);

    distances.set(start, 0);
    heap.push(State {cost: 0, position: start});

    while let Some(State {cost, position}) = heap.pop() {
        if &cost > distances.get(position) { continue; }

        for edge in get_neighbours(&mut walls, position) {
            let next = State{cost: cost + 1, position: edge};

            if &next.cost < distances.get(edge) {
                path.insert(next.position, position);
                heap.push(next);
                distances.set(next.position, next.cost)
            }
        }
    }
    distances.data.into_iter().filter(|&(_, v)| v <= 50).count()
}

fn is_wall(x: usize, y: usize, num: usize) -> bool {
    let a: usize = x*x + 3*x + 2*x*y + y + y*y + num;
    format!("{:b}", a)          // Format as binary string
        .chars()                // Get the chars
        .filter(|a| a == &'1')  // Filter '1's out
        .count()                // Count them
        % 2 == 1                // See if there is an odd amount
}

fn get_neighbours(walls: &mut DefaultMap<Node, bool>, (x, y): Node) -> Vec<Node>{
    let mut neighbours: Vec<Node> = Vec::new();
    let dirs: Vec<(isize, isize)> = vec![(-1, 0), (1, 0), (0, -1), (0, 1)];

    for (dx, dy) in dirs {
        let temp_x = x as isize + dx;
        let temp_y = y as isize + dy;

        if temp_x.is_negative() || temp_y.is_negative() {
            continue;
        }
        if ! walls.get((temp_x as usize, temp_y as usize)) {
            neighbours.push((temp_x as usize, temp_y as usize));
        }

    }
    neighbours
}

fn draw_board(walls: &mut DefaultMap<Node, bool>, path: Vec<Node>) {
    let mut max_x = 0;
    let mut max_y = 0;

    for (x, y) in walls.data.keys() {
        max_x = cmp::max(*x, max_x);
        max_y = cmp::max(*y, max_y);
    }

    for y in 0..max_y+1 {
        for x in 0..max_x+1 {
            if path.contains(&(x, y)) {
                print!("O");
            } else if ! walls.data.contains_key(&(x, y)) {
                print!(" ");
            } else {
                print!("{}", if *walls.get((x, y)) {'#'} else {'.'})
            }
        }
        println!();
    }

}