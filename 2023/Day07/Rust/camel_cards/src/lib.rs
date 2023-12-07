use std::{cmp::Reverse, collections::HashMap};

type Input<'a> = Vec<(Vec<u8>, usize)>;

const T: u8 = 10;
const J: u8 = 11;
const Q: u8 = 12;
const K: u8 = 13;
const A: u8 = 14;

fn add_type((hand, bid): &(Vec<u8>, usize), replace_jokers: bool) -> (Vec<u8>, usize) {
    let mut count = HashMap::with_capacity(5);
    for x in hand {
        *(count.entry(x).or_insert(0)) += 1;
    }
    if replace_jokers && count.contains_key(&J) {
        let mut keys: Vec<_> = count.keys().collect();
        keys.sort_by_key(|c| Reverse(count.get(*c).unwrap()));
        if let Some(to_replace) = keys.iter().find(|&&&&x| x != J) {
            let to_add = *count.get(&J).unwrap();
            count.entry(to_replace).and_modify(|c| *c += to_add);
            count.remove(&J);
        }
    }

    let mut count: Vec<_> = count.values().collect();
    count.sort();

    (
        match count[..] {
            [5] => [7],
            [1, 4] => [6],
            [2, 3] => [5],
            [1, 1, 3] => [4],
            [1, 2, 2] => [3],
            [1, 1, 1, 2] => [2],
            [1, 1, 1, 1, 1] => [1],
            _ => unreachable!(),
        }
        .iter()
        .chain(
            hand.iter()
                .map(|x| if replace_jokers && *x == J { &0 } else { x }),
        )
        .map(std::borrow::ToOwned::to_owned)
        .collect(),
        *bid,
    )
}

/// Parser for 2023 Day 07 (`camel_cards`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let (hand, bid) = l.split_once(' ').unwrap();
            (
                hand.bytes()
                    .map(|x| match x {
                        b'T' => T,
                        b'J' => J,
                        b'Q' => Q,
                        b'K' => K,
                        b'A' => A,
                        _ => x - b'0',
                    })
                    .collect(),
                bid.parse().unwrap(),
            )
        })
        .collect()
}

/// Solver for part 1 of 2023 Day 07 (`camel_cards`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut hands: Vec<_> = input.iter().map(|x| add_type(x, false)).collect();
    hands.sort();
    hands
        .iter()
        .enumerate()
        .map(|(i, (_, bid))| (i + 1) * bid)
        .sum()
}

/// Solver for part 2 of 2023 Day 07 (`camel_cards`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut hands: Vec<_> = input.iter().map(|x| add_type(x, true)).collect();
    hands.sort();
    hands
        .iter()
        .enumerate()
        .map(|(i, (_, bid))| (i + 1) * bid)
        .sum()
}
