use std::{cmp::Ordering, collections::BinaryHeap, fs};
#[derive(Debug, Clone, PartialEq, Eq)]
struct Unit {
    unit_type: UnitType,
    pos: (usize, usize),
    health: isize,
}

impl Unit {
    fn step(&mut self, grid: &[Vec<char>], units: &[Unit]) -> Option<usize> {
        // Getting Targets
        let dists = get_dist(self.pos, grid, units);
        let mut possible_targets = Vec::with_capacity(4 * units.len());

        let directions = vec![(0, -1), (-1, 0), (1, 0), (0, 1)];
        for u in units {
            if u.unit_type != self.unit_type && u.health > 0 {
                for (dx, dy) in &directions {
                    let x = (u.pos.0 as isize + dx) as usize;
                    let y = (u.pos.1 as isize + dy) as usize;
                    possible_targets.push((dists[x][y], (x, y)));
                }
            }
        }
        possible_targets.retain(|(d, _)| d != &usize::MAX);
        possible_targets.sort_by_key(|(d, _)| *d);
        let target = if !possible_targets.is_empty() {
            possible_targets[0].1
        } else {
            self.pos
        };
        // Moving towards Target
        if target != self.pos {
            let mut possible_moves = Vec::with_capacity(4);
            let target_dists = get_dist(target, grid, units);
            for (dx, dy) in &directions {
                let x = (self.pos.0 as isize + dx) as usize;
                let y = (self.pos.1 as isize + dy) as usize;
                possible_moves.push((target_dists[x][y], (x, y)));
            }
            possible_moves.retain(|(d, _)| d != &usize::MAX);
            possible_moves.sort_by_key(|(d, _)| *d);
            if !possible_moves.is_empty() {
                self.pos = possible_moves[0].1;
            }
        }

        // Attacking
        let mut possible_attacks = Vec::with_capacity(4);
        for (dx, dy) in &directions {
            let x = (self.pos.0 as isize + dx) as usize;
            let y = (self.pos.1 as isize + dy) as usize;
            for (i, u) in units.iter().enumerate() {
                if u.unit_type != self.unit_type && u.pos == (x, y) && u.health > 0 {
                    possible_attacks.push((u.health, i));
                }
            }
        }
        possible_attacks.sort_by_key(|(d, _)| *d);
        if !possible_attacks.is_empty() {
            return Some(possible_attacks[0].1);
        }
        None
    }
}

impl Ord for Unit {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.pos.1 != other.pos.1 {
            return self.pos.1.cmp(&other.pos.1);
        }
        self.pos.0.cmp(&other.pos.0)
    }
}

impl PartialOrd for Unit {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum UnitType {
    Goblin,
    Elf,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct State {
    cost: usize,
    position: (usize, usize),
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

type Input = (Vec<Vec<char>>, Vec<Unit>);

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");

    println!("Part 1: {}", part_1(&raw_input));
    println!("Part 2: {}", part_2(&raw_input));
}

fn parse(input: &str) -> Input {
    let mut grid = Vec::new();
    let mut units = Vec::new();
    for (y, row) in input.trim().split('\n').enumerate() {
        let mut row_vec = Vec::new();
        for (x, c) in row.char_indices() {
            match c {
                'E' => {
                    units.push(Unit {
                        unit_type: UnitType::Elf,
                        pos: (x, y),
                        health: 200,
                    });
                    row_vec.push('.')
                }
                'G' => {
                    units.push(Unit {
                        unit_type: UnitType::Goblin,
                        pos: (x, y),
                        health: 200,
                    });
                    row_vec.push('.')
                }
                _ => row_vec.push(c),
            }
        }
        grid.push(row_vec);
    }
    units.sort();
    (grid, units)
}

fn part_1(input: &str) -> usize {
    let (mut units, round) = battle(input, 3);
    units.retain(|u| u.health > 0);
    round * units.iter().map(|u| u.health as usize).sum::<usize>()
}

fn part_2(input: &str) -> usize {
    for p in 4.. {
        let (mut units, round) = battle(input, p);
        if units
            .iter()
            .all(|u| u.health > 0 || u.unit_type != UnitType::Elf)
        {
            units.retain(|u| u.health > 0);
            return round * units.iter().map(|u| u.health as usize).sum::<usize>();
        }
    }
    unreachable!()
}

fn battle(input: &str, elf_power: isize) -> (Vec<Unit>, usize) {
    let mut data = parse(input);
    for round in 0.. {
        data.1.sort();
        println!("Round: {}", round);
        _display(&data);
        for index in 0..data.1.len() {
            if data.1[index].health < 0 {
                continue;
            }
            if get_winner(&data.1) {
                return (data.1.to_owned(), round);
            }

            let units = data.1.clone();
            if let Some(i) = data.1[index].step(&data.0, &units) {
                data.1[i].health -= if data.1[index].unit_type == UnitType::Elf {
                    elf_power
                } else {
                    3
                };
            }
        }
    }
    unreachable!()
}

fn _display((grid, units): &Input) {
    for (y, row) in grid.iter().enumerate() {
        for (x, c) in row.iter().enumerate() {
            let mut unit = false;
            for u in units {
                if u.pos == (x, y) && u.health > 0 {
                    unit = true;
                    match u.unit_type {
                        UnitType::Goblin => print!("G"),
                        UnitType::Elf => print!("E"),
                    }
                    break;
                }
            }
            if !unit {
                print!("{}", c);
            }
        }
        for u in units {
            if u.pos.1 == y && u.health > 0 {
                print!(
                    " {}({})",
                    match u.unit_type {
                        UnitType::Goblin => "G",
                        UnitType::Elf => "E",
                    },
                    u.health
                )
            }
        }
        println!();
    }

    println!("\n");
}

fn get_dist(start: (usize, usize), grid: &[Vec<char>], units: &[Unit]) -> Vec<Vec<usize>> {
    let mut heap: BinaryHeap<State> = BinaryHeap::new();
    let mut distances = vec![vec![usize::MAX; grid.len()]; grid[0].len()]; // Initialise all positions on grid to be infinite
    let mut queued = vec![vec![false; grid.len()]; grid[0].len()]; // Keep track of places visited
    let directions = vec![(0, -1), (-1, 0), (1, 0), (0, 1)];

    for (y, row) in grid.iter().enumerate() {
        for (x, c) in row.iter().enumerate() {
            if c == &'#' {
                queued[x][y] = true; // Add walls to visited, sets distance to Inf.
            }
        }
    }
    for u in units {
        if u.pos == start || u.health < 0 {
            continue;
        }
        let (x, y) = u.pos;
        queued[x][y] = true; // Set positions blocked by units that are not ourselves to be Inf.
    }

    heap.push(State {
        cost: 0,
        position: start, // Add current unit pos to heap
    });
    queued[start.0][start.1] = true;

    while let Some(State {
        cost,
        position: (x, y),
    }) = heap.pop()
    {
        distances[x][y] = cost;
        for (dx, dy) in &directions {
            let x = (x as isize + dx) as usize;
            let y = (y as isize + dy) as usize;
            if !queued[x][y] {
                queued[x][y] = true;
                heap.push(State {
                    cost: cost + 1,
                    position: (x, y),
                });
            }
        }
    }

    distances
}

fn get_winner(units: &[Unit]) -> bool {
    for unit_type in [UnitType::Goblin, UnitType::Elf] {
        if units
            .iter()
            .all(|a| a.unit_type == unit_type || a.health < 0)
        {
            return true;
        }
    }
    false
}
