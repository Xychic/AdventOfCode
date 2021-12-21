use itertools::Itertools;
use std::{collections::HashMap, fs, time::Instant};

type Input = [[usize; 2]; 2];

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
    let mut lines = input.trim().lines().map(|l| l.split(' '));
    [
        [
            lines
                .next()
                .unwrap()
                .next_back()
                .unwrap()
                .parse::<usize>()
                .unwrap()
                - 1,
            0,
        ],
        [
            lines
                .next()
                .unwrap()
                .next_back()
                .unwrap()
                .parse::<usize>()
                .unwrap()
                - 1,
            0,
        ],
    ]
}

fn part_1(input: &Input) -> usize {
    let mut state = input.to_owned();
    let dice: Vec<_> = (1..=100).collect();
    let mut dice_rolls = 0;
    let mut player = 0;
    loop {
        if state[0][1] >= 1000 {
            return state[1][1] * dice_rolls;
        } else if state[1][1] >= 1000 {
            return state[0][1] * dice_rolls;
        }

        let mut steps = 0;
        for _ in 0..3 {
            steps += dice[dice_rolls % 100];
            dice_rolls += 1;
        }

        let pos = (state[player][0] + steps) % 10;
        state[player][0] = pos;
        state[player][1] += pos + 1;
        player += 1;
        player %= 2;
    }
}

fn part_2(input: &Input) -> usize {
    *count_wins(input, 0, &mut HashMap::with_capacity(10 * 10 * 21 * 21))
        .iter()
        .max()
        .unwrap()
}

fn count_wins(
    current_state: &Input,
    current_player: usize,
    seen_states: &mut HashMap<(Input, usize), [usize; 2]>,
) -> [usize; 2] {
    if current_state[0][1] >= 21 {
        return [1, 0];
    } else if current_state[1][1] >= 21 {
        return [0, 1];
    } else if let Some(&ans) = seen_states.get(&(*current_state, current_player)) {
        return ans;
    }
    let mut wins = [0, 0];
    let dice: [[usize; 3]; 3] = [[1, 2, 3], [1, 2, 3], [1, 2, 3]];
    for rolls in dice.iter().multi_cartesian_product() {
        let mut new_state = current_state.to_owned();
        new_state[current_player][0] =
            (current_state[current_player][0] + rolls.iter().map(|&&x| x).sum::<usize>()) % 10;
        new_state[current_player][1] += new_state[current_player][0] + 1;
        let new_wins = count_wins(&new_state, (current_player + 1) % 2, seen_states);
        wins[0] += new_wins[0];
        wins[1] += new_wins[1];
    }

    seen_states.insert((*current_state, current_player), wins);

    wins
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "
Player 1 starting position: 4
Player 2 starting position: 8";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 739785);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 444356092776315);
    }
}
