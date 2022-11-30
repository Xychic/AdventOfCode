use std::{collections::HashSet, fs, time::Instant};

use priority_queue::PriorityQueue;

type Input<'a> = Character;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum MoveOptionID {
    Attack = 0,
    MagicMissile = 1,
    Drain = 2,
    Shield = 3,
    Poison = 4,
    Recharge = 5,
}

#[allow(dead_code)]
struct MoveOption<'a> {
    id: MoveOptionID,
    name: &'a str,
    required_damage: usize,
    required_mana: usize,
}

const MOVE_OPTIONS: [MoveOption; 6] = [
    MoveOption {
        id: MoveOptionID::Attack,
        required_damage: 1,
        required_mana: 0,
        name: "Attack",
    },
    MoveOption {
        id: MoveOptionID::MagicMissile,
        required_damage: 0,
        required_mana: 53,
        name: "Magic Missile",
    },
    MoveOption {
        id: MoveOptionID::Drain,
        required_damage: 0,
        required_mana: 73,
        name: "Drain",
    },
    MoveOption {
        id: MoveOptionID::Shield,
        required_damage: 0,
        required_mana: 113,
        name: "Shield",
    },
    MoveOption {
        id: MoveOptionID::Poison,
        required_damage: 0,
        required_mana: 173,
        name: "Poison",
    },
    MoveOption {
        id: MoveOptionID::Recharge,
        required_damage: 0,
        required_mana: 229,
        name: "Recharge",
    },
];

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct State {
    player: Character,
    boss: Character,
    player_turn: bool,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct Character {
    health: usize,
    damage: usize,
    mana: usize,
    shield_time: usize,
    poison_time: usize,
    recharge_time: usize,
}

impl Character {
    fn new_boss(health: usize, damage: usize) -> Character {
        Character::new(health, damage, 0)
    }

    fn new(health: usize, damage: usize, mana: usize) -> Character {
        Character {
            health,
            damage,
            mana,
            shield_time: 0,
            poison_time: 0,
            recharge_time: 0,
        }
    }

    fn step(&mut self) {
        if self.poison_time > 0 {
            self.health = self.health.saturating_sub(3);
            self.poison_time -= 1;
        }
        if self.shield_time > 0 {
            self.shield_time -= 1;
        }
        if self.recharge_time > 0 {
            self.mana += 101;
            self.recharge_time -= 1;
        }
    }

    fn get_options(&self, target: &Character) -> Vec<MoveOptionID> {
        let mut options = Vec::with_capacity(6);
        for (index, move_opt) in MOVE_OPTIONS.iter().enumerate() {
            assert_eq!(index, move_opt.id as usize);
            if self.damage >= move_opt.required_damage && self.mana >= move_opt.required_mana {
                if (move_opt.id == MoveOptionID::Recharge && self.recharge_time > 0)
                    || (move_opt.id == MoveOptionID::Poison && target.poison_time > 0)
                    || (move_opt.id == MoveOptionID::Shield && self.shield_time > 0)
                {
                    continue;
                }
                options.push(move_opt.id);
            }
        }
        options
    }

    fn use_move(&mut self, target: &mut Character, move_option: MoveOptionID) -> usize {
        match move_option {
            MoveOptionID::Attack => self.attack(target),
            MoveOptionID::MagicMissile => self.cast_magic_missile(target),
            MoveOptionID::Drain => self.cast_drain(target),
            MoveOptionID::Shield => self.cast_shield(),
            MoveOptionID::Poison => self.cast_poison(target),
            MoveOptionID::Recharge => self.cast_recharge(),
        }
    }

    fn attack(&mut self, target: &mut Character) -> usize {
        let move_ = &MOVE_OPTIONS[MoveOptionID::Attack as usize];
        assert_eq!(MoveOptionID::Attack, move_.id);
        assert!(self.damage >= move_.required_damage);

        let damage_done = self
            .damage
            .checked_sub(if target.shield_time > 0 { 7 } else { 0 })
            .unwrap_or(1);
        target.health = target.health.saturating_sub(damage_done);

        move_.required_mana
    }

    fn cast_magic_missile(&mut self, target: &mut Character) -> usize {
        let move_ = &MOVE_OPTIONS[MoveOptionID::MagicMissile as usize];
        assert_eq!(MoveOptionID::MagicMissile, move_.id);
        assert!(self.mana >= move_.required_mana);

        self.mana -= move_.required_mana;
        target.health -= 4;
        move_.required_mana
    }

    fn cast_drain(&mut self, target: &mut Character) -> usize {
        let move_ = &MOVE_OPTIONS[MoveOptionID::Drain as usize];
        assert_eq!(MoveOptionID::Drain, move_.id);
        assert!(self.mana >= move_.required_mana);

        self.mana -= move_.required_mana;
        target.health -= 2;
        self.health += 2;
        move_.required_mana
    }

    fn cast_shield(&mut self) -> usize {
        let move_ = &MOVE_OPTIONS[MoveOptionID::Shield as usize];
        assert_eq!(MoveOptionID::Shield, move_.id);
        assert!(self.mana >= move_.required_mana);

        assert!(self.shield_time == 0);
        self.mana -= move_.required_mana;
        self.shield_time += 6;
        move_.required_mana
    }

    fn cast_poison(&mut self, target: &mut Character) -> usize {
        let move_ = &MOVE_OPTIONS[MoveOptionID::Poison as usize];
        assert_eq!(MoveOptionID::Poison, move_.id);
        assert!(self.mana >= move_.required_mana);

        assert!(target.poison_time == 0);
        self.mana -= move_.required_mana;
        target.poison_time += 6;
        move_.required_mana
    }

    fn cast_recharge(&mut self) -> usize {
        let move_ = &MOVE_OPTIONS[MoveOptionID::Recharge as usize];
        assert_eq!(MoveOptionID::Recharge, move_.id);
        assert!(self.mana >= move_.required_mana);

        assert!(self.recharge_time == 0);
        self.mana -= move_.required_mana;
        self.recharge_time += 5;
        move_.required_mana
    }
}

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
    // inpt(input.trim()).unwrap()
    let (hp_line, dmg_line) = input.trim().split_once('\n').unwrap();
    let hp = hp_line.split_once(": ").unwrap().1.parse().unwrap();
    let dmg = dmg_line.split_once(": ").unwrap().1.parse().unwrap();
    Character::new_boss(hp, dmg)
}

fn simulate(boss: &Character, player: &Character, hard_mode: bool) -> usize {
    let mut queue = PriorityQueue::new();
    let mut seen = HashSet::new();

    let initial = State {
        player: player.to_owned(),
        boss: boss.to_owned(),
        player_turn: true,
    };

    queue.push(initial, usize::MAX);
    seen.insert(initial);

    while let Some((mut state, mana)) = queue.pop() {
        state.boss.step();
        state.player.step();
        if hard_mode && state.player_turn {
            state.player.health = state.player.health.saturating_sub(1);
        }
        if state.boss.health == 0 {
            return usize::MAX - mana;
        } else if state.player.health == 0 {
            continue;
        }
        if state.player_turn {
            for move_option in state.player.get_options(&state.boss) {
                let mut new = state;
                new.player_turn = false;
                let spent = new.player.use_move(&mut new.boss, move_option);

                if !seen.contains(&new) {
                    queue.push(new, mana - spent);
                }
            }
        } else {
            for move_option in state.boss.get_options(&state.player) {
                let mut new = state;
                new.player_turn = true;
                new.boss.use_move(&mut new.player, move_option);

                if !seen.contains(&new) {
                    queue.push(new, mana);
                }
            }
        }
    }

    unreachable!()
}

fn part_1(boss: &Input) -> usize {
    simulate(boss, &Character::new(50, 0, 500), false)
}

fn part_2(boss: &Input) -> usize {
    simulate(boss, &Character::new(50, 0, 500), true)
}
