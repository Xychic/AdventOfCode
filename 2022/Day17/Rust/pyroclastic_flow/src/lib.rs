use std::collections::{HashMap, HashSet};

type Input<'a> = Vec<char>;

#[derive(Debug, Clone, Copy)]
struct RockPart<const S: usize> {
    data: [(usize, usize); S],
}

impl<const S: usize> RockPart<S> {
    fn translate(&mut self, (dx, dy): (isize, isize), rocks: &HashSet<(usize, usize)>) -> bool {
        let new: Vec<_> = self
            .data
            .iter()
            .map(|&(x, y)| ((x as isize + dx) as usize, (y as isize + dy) as usize))
            .collect();
        if new.iter().all(|&(x, _)| x <= 6) {
            if new.iter().any(|p| rocks.contains(p)) {
                false
            } else {
                self.data[..S].copy_from_slice(&new[..S]);
                true
            }
        } else {
            true
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Rock {
    Horizontal(RockPart<4>),
    Cross(RockPart<5>),
    L(RockPart<5>),
    Vertical(RockPart<4>),
    Square(RockPart<4>),
}

impl Rock {
    fn translate(&mut self, delta: (isize, isize), rocks: &HashSet<(usize, usize)>) -> bool {
        match self {
            Rock::Horizontal(data) => data.translate(delta, rocks),
            Rock::Cross(data) => data.translate(delta, rocks),
            Rock::L(data) => data.translate(delta, rocks),
            Rock::Vertical(data) => data.translate(delta, rocks),
            Rock::Square(data) => data.translate(delta, rocks),
        }
    }

    fn get_data(&self) -> &[(usize, usize)] {
        match self {
            Rock::Horizontal(rock_part) => &rock_part.data,
            Rock::Cross(rock_part) => &rock_part.data,
            Rock::L(rock_part) => &rock_part.data,
            Rock::Vertical(rock_part) => &rock_part.data,
            Rock::Square(rock_part) => &rock_part.data,
        }
    }

    fn step_left(&mut self, rocks: &HashSet<(usize, usize)>) -> bool {
        self.translate((-1, 0), rocks)
    }

    fn step_right(&mut self, rocks: &HashSet<(usize, usize)>) -> bool {
        self.translate((1, 0), rocks)
    }

    fn step_down(&mut self, rocks: &HashSet<(usize, usize)>) -> bool {
        self.translate((0, -1), rocks)
    }
}

const ROCKS: [Rock; 5] = [
    Rock::Horizontal(RockPart {
        data: [(0, 0), (1, 0), (2, 0), (3, 0)],
    }),
    Rock::Cross(RockPart {
        data: [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
    }),
    Rock::L(RockPart {
        data: [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
    }),
    Rock::Vertical(RockPart {
        data: [(0, 0), (0, 1), (0, 2), (0, 3)],
    }),
    Rock::Square(RockPart {
        data: [(0, 0), (1, 0), (0, 1), (1, 1)],
    }),
];

pub fn parse(input: &str) -> Input {
    input.trim().chars().collect()
}

pub fn show(data: &HashSet<(usize, usize)>) {
    let max_y = *data.iter().map(|(_, y)| y).max().unwrap();
    for y in (0..=max_y).rev() {
        let mut line = String::with_capacity(7);
        for x in 0..7 {
            line += if data.contains(&(x, y)) { "#" } else { "." };
        }
        println!("{line}");
    }
    println!();
}

pub fn show_falling(data: &HashSet<(usize, usize)>, rock: &[(usize, usize)]) {
    let mut max_y = *data.iter().map(|(_, y)| y).max().unwrap();
    for &(_, y) in rock {
        max_y = max_y.max(y);
    }

    for y in (0..=max_y).rev() {
        let mut line = String::with_capacity(7);
        for x in 0..7 {
            line += if data.contains(&(x, y)) {
                "#"
            } else if rock.contains(&(x, y)) {
                "@"
            } else {
                "."
            };
        }
        println!("{line}");
    }
    println!();
}

pub fn part_1(input: &Input) -> usize {
    let mut jets = input.iter().cycle();

    let mut rock_cycle = ROCKS.iter().cycle();
    let mut rock_set = HashSet::new();

    for x in 0..7 {
        rock_set.insert((x, 0));
    }

    for _ in 0..2022 {
        let mut rock = *rock_cycle.next().unwrap();
        let max_height = *rock_set.iter().map(|(_, y)| y).max().unwrap() as isize + 4;
        rock.translate((2, max_height), &rock_set);

        loop {
            let dir = *jets.next().unwrap();
            if dir == '<' {
                rock.step_left(&rock_set);
            } else {
                assert!(dir == '>');
                rock.step_right(&rock_set);
            }
            if !rock.step_down(&rock_set) {
                for &p in rock.get_data() {
                    rock_set.insert(p);
                }
                break;
            }
        }
    }

    *rock_set.iter().map(|(_, y)| y).max().unwrap()
}

pub fn part_2(input: &Input) -> usize {
    let mut rock_set = HashSet::new();

    for x in 0..7 {
        rock_set.insert((x, 0));
    }

    let mut seen = HashMap::new();

    let mut jet_index = 0;
    let mut rock_index = 0;
    let mut fallen = 0;

    let mut max_height = 0;

    let target: usize = 1_000_000_000_000;

    while fallen < target {
        let mut rock = ROCKS[rock_index % ROCKS.len()].to_owned();
        rock.translate((2, max_height as isize + 4), &rock_set);
        rock_index += 1;
        rock_index %= ROCKS.len();

        loop {
            if input[jet_index] == '<' {
                rock.step_left(&rock_set);
            } else {
                rock.step_right(&rock_set);
            }

            jet_index += 1;
            jet_index %= input.len();

            if !rock.step_down(&rock_set) {
                for &p in rock.get_data() {
                    rock_set.insert(p);
                }
                max_height = rock
                    .get_data()
                    .iter()
                    .map(|&(_, y)| y)
                    .max()
                    .unwrap()
                    .max(max_height);
                break;
            }
        }
        let key = (rock_index, jet_index);

        if let Some(&(prev_fallen, height)) = seen.get(&key) {
            let period = fallen - prev_fallen;
            let period_change = max_height - height;
            let periods_left = (target - fallen) / period;
            if fallen % period == target % period {
                return max_height + period_change * periods_left - 1;
            }
        } else {
            seen.insert(key, (fallen, max_height));
        }
        fallen += 1;
    }

    max_height
}
