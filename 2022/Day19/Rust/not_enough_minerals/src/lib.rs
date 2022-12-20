use std::collections::{HashSet, VecDeque};

#[derive(Debug)]
pub struct Blueprint {
    id: usize,
    ore_cost: usize,
    clay_cost: usize,
    obsidian_cost: (usize, usize),
    geode_cost: (usize, usize),
}

impl Blueprint {
    fn from_line(line: &str) -> Self {
        let words: Vec<_> = line.split(' ').collect();
        let id = words[1].strip_suffix(':').unwrap().parse().unwrap();
        let ore_cost = words[6].parse().unwrap();
        let clay_cost = words[12].parse().unwrap();
        let obsidian_cost = (words[18].parse().unwrap(), words[21].parse().unwrap());
        let geode_cost = (words[27].parse().unwrap(), words[30].parse().unwrap());

        Blueprint {
            id,
            ore_cost,
            clay_cost,
            obsidian_cost,
            geode_cost,
        }
    }
}

type Input<'a> = Vec<Blueprint>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct State {
    ore: usize,
    clay: usize,
    obsidian: usize,
    geodes: usize,
    ore_robots: usize,
    clay_robots: usize,
    obsidian_robots: usize,
    geode_robots: usize,
    time: usize,
}

impl State {
    fn get_next(&self) -> Self {
        State {
            ore: self.ore + self.ore_robots,
            clay: self.clay + self.clay_robots,
            obsidian: self.obsidian + self.obsidian_robots,
            geodes: self.geodes + self.geode_robots,
            ore_robots: self.ore_robots,
            clay_robots: self.clay_robots,
            obsidian_robots: self.obsidian_robots,
            geode_robots: self.geode_robots,
            time: self.time - 1,
        }
    }
}

fn get_total_geodes(blueprint: &Blueprint, time: usize) -> usize {
    let mut best_geode_count = 0;
    let mut queue = VecDeque::new();
    let mut seen = HashSet::new();

    queue.push_back(State {
        ore: 0,
        clay: 0,
        obsidian: 0,
        geodes: 0,
        ore_robots: 1,
        clay_robots: 0,
        obsidian_robots: 0,
        geode_robots: 0,
        time,
    });
    let core = blueprint
        .ore_cost
        .max(blueprint.clay_cost)
        .max(blueprint.obsidian_cost.0)
        .max(blueprint.geode_cost.0);

    while let Some(mut state) = queue.pop_front() {
        if state.time == 0 {
            best_geode_count = best_geode_count.max(state.geodes);
            continue;
        }

        state.ore_robots = state.ore_robots.min(core);
        state.clay_robots = state.clay_robots.min(blueprint.obsidian_cost.1);
        state.obsidian_robots = state.obsidian_robots.min(blueprint.geode_cost.1);
        state.ore = state
            .ore
            .min(state.time * core - state.ore * (state.time - 1));
        state.clay = state
            .clay
            .min(state.time * blueprint.obsidian_cost.1 - state.clay_robots * (state.time - 1));
        state.obsidian = state
            .obsidian
            .min(state.time * blueprint.geode_cost.1 - state.obsidian_robots * (state.time - 1));

        if seen.contains(&state) {
            continue;
        }
        seen.insert(state);

        if seen.len() % 1_000_000 == 0 {
            println!("time: {} seen: {}", state.time, seen.len());
        }
        queue.push_back(state.get_next());

        if state.ore >= blueprint.ore_cost {
            let mut new_state = state.get_next();
            new_state.ore -= blueprint.ore_cost;
            new_state.ore_robots += 1;
            queue.push_back(new_state);
        }
        if state.ore >= blueprint.clay_cost {
            let mut new_state = state.get_next();
            new_state.ore -= blueprint.clay_cost;
            new_state.clay_robots += 1;
            queue.push_back(new_state);
        }
        if state.ore >= blueprint.obsidian_cost.0 && state.clay >= blueprint.obsidian_cost.1 {
            let mut new_state = state.get_next();
            new_state.ore -= blueprint.obsidian_cost.0;
            new_state.clay -= blueprint.obsidian_cost.1;
            new_state.obsidian_robots += 1;
            queue.push_back(new_state);
        }
        if state.ore >= blueprint.geode_cost.0 && state.obsidian >= blueprint.geode_cost.1 {
            let mut new_state = state.get_next();
            new_state.ore -= blueprint.geode_cost.0;
            new_state.obsidian -= blueprint.geode_cost.1;
            new_state.geode_robots += 1;
            queue.push_back(new_state);
        }
    }

    best_geode_count
}

pub fn parse(input: &str) -> Input {
    input.trim().lines().map(Blueprint::from_line).collect()
}

pub fn part_1(input: &Input) -> usize {
    input.iter().map(|b| b.id * get_total_geodes(b, 24)).sum()
}

pub fn part_2(input: &Input) -> usize {
    input
        .iter()
        .take(3)
        .map(|b| get_total_geodes(b, 32))
        .fold(1, |acc, x| acc * x)
}
