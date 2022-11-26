use inpt::{inpt, Inpt};
use itertools::Itertools;
use std::{fs, time::Instant};

type Input<'a> = Character;

#[derive(Inpt, Debug, Clone, Copy)]
#[inpt(regex = r"Hit Points: (\d+)\nDamage: (\d+)\nArmor: (\d+)")]
struct Character {
    health: usize,
    damage: usize,
    armour: usize,
}

impl Character {
    fn receive_attack(&mut self, foe: &Character) {
        let damage_done = foe.damage.checked_sub(self.armour).unwrap_or(1);
        self.health = self.health.saturating_sub(damage_done);
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Item<'a> {
    name: &'a str,
    cost: usize,
    damage: usize,
    armour: usize,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Loadout<'a> {
    weapon: Option<Item<'a>>,
    armour: Option<Item<'a>>,
    ring_1: Option<Item<'a>>,
    ring_2: Option<Item<'a>>,
}

impl Loadout<'_> {
    fn get_cost(&self) -> usize {
        let mut cost = 0;
        if let Some(weapon) = self.weapon {
            cost += weapon.cost;
        }
        if let Some(armour) = self.armour {
            cost += armour.cost;
        }
        if let Some(ring_1) = self.ring_1 {
            cost += ring_1.cost;
        }
        if let Some(ring_2) = self.ring_2 {
            cost += ring_2.cost;
        }
        cost
    }

    fn get_damage(&self) -> usize {
        let mut damage = 0;
        if let Some(weapon) = self.weapon {
            damage += weapon.damage;
        }
        if let Some(armour) = self.armour {
            damage += armour.damage;
        }
        if let Some(ring_1) = self.ring_1 {
            damage += ring_1.damage;
        }
        if let Some(ring_2) = self.ring_2 {
            damage += ring_2.damage;
        }
        damage
    }

    fn get_armour(&self) -> usize {
        let mut armour_amount = 0;
        if let Some(weapon) = self.weapon {
            armour_amount += weapon.armour;
        }
        if let Some(armour) = self.armour {
            armour_amount += armour.armour;
        }
        if let Some(ring_1) = self.ring_1 {
            armour_amount += ring_1.armour;
        }
        if let Some(ring_2) = self.ring_2 {
            armour_amount += ring_2.armour;
        }
        armour_amount
    }
}

const WEAPONS: [Item; 5] = [
    Item {
        name: "Dagger",
        cost: 8,
        damage: 4,
        armour: 0,
    },
    Item {
        name: "Shortsword",
        cost: 10,
        damage: 5,
        armour: 0,
    },
    Item {
        name: "Warhammer",
        cost: 25,
        damage: 6,
        armour: 0,
    },
    Item {
        name: "Longsword",
        cost: 40,
        damage: 7,
        armour: 0,
    },
    Item {
        name: "Greataxe",
        cost: 74,
        damage: 8,
        armour: 0,
    },
];

const ARMOUR: [Item; 6] = [
    Item {
        name: "None",
        cost: 0,
        damage: 0,
        armour: 0,
    },
    Item {
        name: "Leather",
        cost: 13,
        damage: 0,
        armour: 1,
    },
    Item {
        name: "Chainmail",
        cost: 31,
        damage: 0,
        armour: 2,
    },
    Item {
        name: "Splintmail",
        cost: 53,
        damage: 0,
        armour: 3,
    },
    Item {
        name: "Bandedmail",
        cost: 75,
        damage: 0,
        armour: 4,
    },
    Item {
        name: "Platemail",
        cost: 102,
        damage: 0,
        armour: 5,
    },
];

const RINGS: [Item; 8] = [
    Item {
        name: "None",
        cost: 0,
        damage: 0,
        armour: 0,
    },
    Item {
        name: "None",
        cost: 0,
        damage: 0,
        armour: 0,
    },
    Item {
        name: "Damage +1",
        cost: 25,
        damage: 1,
        armour: 0,
    },
    Item {
        name: "Damage +2",
        cost: 50,
        damage: 2,
        armour: 0,
    },
    Item {
        name: "Damage +3",
        cost: 100,
        damage: 3,
        armour: 0,
    },
    Item {
        name: "Defense +1",
        cost: 20,
        damage: 0,
        armour: 1,
    },
    Item {
        name: "Defense +2",
        cost: 40,
        damage: 0,
        armour: 2,
    },
    Item {
        name: "Defense +3",
        cost: 80,
        damage: 0,
        armour: 3,
    },
];

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    let start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1(&input),
        Instant::now() - start
    );
    let start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2(&input),
        Instant::now() - start
    );
}

fn parse(input: &str) -> Input {
    inpt(input.trim()).unwrap()
}

fn simulate(player: &Character, boss: &Character) -> bool {
    let mut player = player.to_owned();
    let mut boss = boss.to_owned();

    let mut player_go = true;

    while player.health > 0 && boss.health > 0 {
        if player_go {
            boss.receive_attack(&player);
        } else {
            player.receive_attack(&boss);
        }
        player_go = !player_go;
    }

    !player_go
}

fn generate_all_loadouts() -> Vec<Loadout<'static>> {
    let mut equipment_options: Vec<_> = RINGS
        .iter()
        .tuple_combinations()
        .unique()
        .map(|(a, b)| Loadout {
            weapon: None,
            armour: None,
            ring_1: Some(a.to_owned()),
            ring_2: Some(b.to_owned()),
        })
        .collect();

    equipment_options = equipment_options
        .iter()
        .cartesian_product(WEAPONS)
        .map(|(e, w)| Loadout {
            weapon: Some(w),
            armour: None,
            ring_1: e.ring_1,
            ring_2: e.ring_2,
        })
        .collect();
    equipment_options
        .iter()
        .cartesian_product(ARMOUR)
        .map(|(e, a)| Loadout {
            weapon: e.weapon,
            armour: Some(a),
            ring_1: e.ring_1,
            ring_2: e.ring_2,
        })
        .collect()
}

fn part_1(boss: &Input) -> usize {
    let mut equipment_options = generate_all_loadouts();
    equipment_options.sort_by_key(|e| e.get_cost());
    for e in equipment_options {
        let player = Character {
            health: 100,
            damage: e.get_damage(),
            armour: e.get_armour(),
        };
        if simulate(&player, boss) {
            return e.get_cost();
        }
    }

    unreachable!()
}

fn part_2(boss: &Input) -> usize {
    let mut equipment_options = generate_all_loadouts();
    equipment_options.sort_by_key(|e| usize::MAX - e.get_cost());
    for e in equipment_options {
        let player = Character {
            health: 100,
            damage: e.get_damage(),
            armour: e.get_armour(),
        };
        if !simulate(&player, boss) {
            return e.get_cost();
        }
    }

    unreachable!()
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    #[test]
    fn test_simulation() {
        let player = Character {
            health: 8,
            damage: 5,
            armour: 5,
        };
        let boss = Character {
            health: 12,
            damage: 7,
            armour: 2,
        };
        assert!(simulate(&player, &boss));
    }

    #[test]
    fn test_generate_loadouts() {
        let all_loadouts = generate_all_loadouts();
        assert_eq!(all_loadouts.len(), 660);
    }

    #[test]
    fn test_all_unique_loadouts() {
        let all_loadouts = generate_all_loadouts();
        let all_loadouts_set: HashSet<&Loadout> = HashSet::from_iter(all_loadouts.iter());
        assert_eq!(all_loadouts.len(), all_loadouts_set.len());
    }
}
