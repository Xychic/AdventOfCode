use std::{cmp::Ordering, collections::{BinaryHeap, HashMap}, fs};

use itertools::Itertools;

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
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input: Vec<Vec<_>> = input.trim().split('\n').map(|x| x.chars().into_iter().collect()).collect();

    let mut targets = Vec::new();
    for (y, line) in input.iter().enumerate() {
        for (x, c) in line.iter().enumerate() {
            if c != &'#' && c != &'.' {
                targets.push((*c, (x, y)));
            }
        }
    }
    targets.sort_by(|(a, _), (b,_)| a.cmp(b));
    let targets: Vec<_> = targets.iter().map(|&(_, a)| a).collect();

    let mut distances = HashMap::new();
    let grid = make_grid(&input);
    for (i, &a) in targets.iter().enumerate() {
        for &b in targets.iter().skip(i+1) {
            let dist = get_dist(&grid, a, b);
            distances.insert((a, b), dist);
            distances.insert((b, a), dist);
        }
    }

    println!("Part 1: {}", part_1(&targets, &distances));
    println!("Part 2: {}", part_2(&targets, &distances));
}

fn part_1(targets: &[Node], distances: &HashMap<(Node, Node), usize>) -> usize {
    targets.iter().skip(1).permutations(targets.len()-1).unique().map(|mut perm| {
        perm.insert(0, &targets[0]);
        perm
            .iter()
            .zip(
                perm
                .iter()
                .skip(1)
            )
            .map(|(&a,&b)| 
                distances.get(&(*a, *b)).unwrap())
            .sum()
    })
    .min()
    .unwrap()
}

fn part_2(targets: &[Node], distances: &HashMap<(Node, Node), usize>) -> usize {
    targets.iter().skip(1).permutations(targets.len()-1).unique().map(|mut perm| {
        perm.insert(0, &targets[0]);
        perm.push(&targets[0]);
        perm
            .iter()
            .zip(
                perm
                .iter()
                .skip(1)
            )
            .map(|(&a,&b)| 
                distances.get(&(*a, *b)).unwrap())
            .sum()
    })
    .min()
    .unwrap()
}

fn make_grid(input: &[Vec<char>]) -> Vec<Vec<bool>> {
    let mut grid = vec![vec![false; input[0].len()]; input.len()];
    for (y, line) in input.iter().enumerate() {
        for (x, c) in line.iter().enumerate() {
            grid[y][x] = c != &'#';
        }
    }
    grid
}

fn _print_grid(grid: &[Vec<bool>]) {
    for row in grid {
        for square in row {
            print!("{}", if *square {'.'} else {'#'})
        }
        println!();
    }
}

fn get_dist(grid: &[Vec<bool>], a: Node, b: Node) -> usize {
    let mut distances: HashMap<Node, usize> = HashMap::new();
    let mut heap: BinaryHeap<State> = BinaryHeap::new();
    let mut path: HashMap<Node, Node> = HashMap::new();
    
    let start = a;
    let end = b;

    distances.insert(start, 0);
    heap.push(State {cost: 0, position: start});

    while let Some(State {cost, position}) = heap.pop() {
        if position == end { 
            return cost; 
        }

        if &cost > distances.get(&position).unwrap_or(&usize::MAX) { continue; }

        for edge in get_neighbours(grid, position) {
            let next = State{cost: cost + 1, position: edge};

            if &next.cost < distances.get(&edge).unwrap_or(&usize::MAX) {
                path.insert(next.position, position);
                heap.push(next);
                distances.insert(next.position, next.cost);
            }
        }
    }
    panic!("No path from {:?} to {:?} found!", a, b)
}

fn get_neighbours(grid: &[Vec<bool>], (x, y): Node) -> Vec<Node> {
    let mut neighbours: Vec<Node> = Vec::new();
    let dirs: Vec<(isize, isize)> = vec![(-1, 0), (1, 0), (0, -1), (0, 1)];

    for (dx, dy) in dirs {
        let temp_x = x as isize + dx;
        let temp_y = y as isize + dy;

        if temp_x.is_negative() || temp_y.is_negative() || temp_x >= grid[0].len() as isize || temp_y >= grid.len() as isize {
            continue;
        }
        if grid[temp_y as usize][temp_x as usize] {
            neighbours.push((temp_x as usize, temp_y as usize));
        }

    }
    neighbours
}
