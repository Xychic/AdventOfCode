use std::{cmp::Ordering, collections::HashSet, fs, time::Instant};

use regex::Regex;

#[derive(Debug, Clone, Eq, PartialEq)]
struct Group {
    id: usize,
    side: String,
    units: usize,
    health: usize,
    weak_to: HashSet<String>,
    immune_to: HashSet<String>,
    attack_strength: usize,
    attack_type: String,
    initiative: usize,
    target: Option<usize>,
}

impl Group {
    fn from_string(id: usize, side: String, string: &str) -> Group {
        let re = Regex::new(r"(?P<units>[0-9]+) units each with (?P<health>[0-9]+) hit points (?:\((?P<a>[a-z ,]+)(?:; )?(?P<b>[a-z ,]+)?\) )?with an attack that does (?P<damage>[0-9]+) (?P<type>[a-z]+) damage at initiative (?P<initiative>[0-9]+)").unwrap();
        let caps = re.captures(string).unwrap();

        let mut weak = HashSet::new();
        let mut immune = HashSet::new();

        for group in ["a", "b"] {
            if let Some(x) = caps.name(group) {
                let x = x.as_str().replace(',', "");
                let x: Vec<_> = x.split(' ').collect();
                if x[0] == "weak" {
                    for item in x.iter().skip(2) {
                        weak.insert(item.to_string());
                    }
                } else {
                    for item in x.iter().skip(2) {
                        immune.insert(item.to_string());
                    }
                }
            }
        }
        Group {
            id,
            side,
            units: caps.name("units").unwrap().as_str().parse().unwrap(),
            health: caps.name("health").unwrap().as_str().parse().unwrap(),
            weak_to: weak,
            immune_to: immune,
            attack_strength: caps.name("damage").unwrap().as_str().parse().unwrap(),
            attack_type: caps.name("type").unwrap().as_str().parse().unwrap(),
            initiative: caps.name("initiative").unwrap().as_str().parse().unwrap(),
            target: None,
        }
    }

    fn effective_power(&self) -> usize {
        self.attack_strength * self.units
    }

    fn get_damage(&self, other: &Group) -> usize {
        if other.immune_to.contains(&self.attack_type) {
            return 0;
        }
        self.effective_power()
            * if other.weak_to.contains(&self.attack_type) {
                2
            } else {
                1
            }
    }

    fn recieve_damage(&mut self, damage: usize) -> usize {
        let loss = (damage / self.health).min(self.units);
        self.units -= loss;
        return loss;
    }
}

impl Ord for Group {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .effective_power()
            .cmp(&self.effective_power())
            .then_with(|| other.initiative.cmp(&self.initiative))
    }
}

impl PartialOrd for Group {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

type Input = Vec<Group>;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");

    let start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1(&raw_input),
        Instant::now() - start
    );
    let start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2(&raw_input),
        Instant::now() - start
    );
}

fn parse(input: &str) -> Input {
    let mut side = "";
    let mut groups = Vec::new();
    let mut index = 1;
    for line in input.trim().split('\n') {
        if line == "" {
            continue;
        } else if line == "Immune System:" {
            side = "immune";
        } else if line == "Infection:" {
            side = "infect";
        } else {
            groups.push(Group::from_string(index, side.to_string(), line));
            index += 1;
        }
    }
    groups
}

fn part_1(input: &str) -> usize {
    let mut groups = parse(input);
    let (_, ans) = get_result(&mut groups);
    ans
}

fn part_2(input: &str) -> usize {
    let groups = parse(input);
    for boost in 1.. {
        let mut to_test = groups.clone();
        for g in to_test.iter_mut() {
            if g.side == "immune" {
                g.attack_strength += boost;
            }
        }
        let (winner, ans) = get_result(&mut to_test);
        if winner == "immune".to_string() {
            return ans;
        }
    }
    unreachable!()
}

fn get_result(groups: &mut Input) -> (String, usize) {
    loop {
        // Targetting
        groups.sort();
        let mut targeted = HashSet::with_capacity(groups.len());
        let targets = groups.clone();
        for g in groups.iter_mut() {
            let mut target: Vec<_> = targets
                .iter()
                .filter(|t| t.side != g.side && !targeted.contains(&(t.side.to_owned(), t.id)))
                .collect();
            target.sort_by(|a, b| {
                g.get_damage(b)
                    .cmp(&g.get_damage(a))
                    .then_with(|| a.cmp(&b))
            });
            if target.len() > 0 && g.get_damage(target[0]) != 0 {
                g.target = Some(target[0].id);
                targeted.insert((target[0].side.to_owned(), target[0].id));
            } else {
                g.target = None;
            }
        }
        // Damaging
        groups.sort_by_key(|g| -(g.initiative as isize));
        let mut dead = Vec::with_capacity(groups.len());
        let to_attack: Vec<_> = groups.iter().map(|g| g.id).collect();
        let mut deaths = false;

        for id in to_attack {
            if dead.contains(&id) {
                continue;
            }
            let attacker = groups.iter().filter(|g| g.id == id).next().unwrap();
            if let Some(target) = attacker.target {
                for i in 0..groups.len() {
                    if groups[i].id == target {
                        let damage = attacker.get_damage(&groups[i]);
                        deaths |= groups[i].recieve_damage(damage) != 0;
                        if groups[i].units == 0 {
                            dead.push(groups[i].id);
                        }
                        break;
                    }
                }
            }
        }
        if !deaths {
            return ("none".to_string(), 0);
        }
        groups.retain(|g| g.units > 0);
        let mut sides = HashSet::with_capacity(2);
        for g in groups.iter() {
            sides.insert(g.side.clone());
        }
        if sides.len() == 1 {
            return (
                sides.iter().next().unwrap().to_string(),
                groups.iter().map(|g| g.units).sum(),
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        let test_input = "Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4";
        let test_answer = 5216;
        assert_eq!(part_1(&test_input), test_answer);
    }

    #[test]
    fn test_part_2() {
        let test_input = "Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4";
        let test_answer = 51;
        assert_eq!(part_2(&test_input), test_answer);
    }
}
