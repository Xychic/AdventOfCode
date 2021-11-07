use std::{collections::VecDeque, fs};
use itertools::Itertools;

#[derive(Debug)]
enum Action {
    SwapPositions { a: usize, b: usize },
    SwapLetters { a: char, b: char },
    Reverse { a: usize, b: usize },
    Move { a: usize, b: usize },
    RotateDir { a: RotateDirection, b: usize },
    RotateBased { a: char },
}

#[derive(Debug)]

enum RotateDirection {
    Left,
    Right,
}

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
//     let input = "swap position 4 with position 0
// swap letter d with letter b
// reverse positions 0 through 4
// rotate left 1 step
// move position 1 to position 4
// move position 3 to position 0
// rotate based on position of letter b
// rotate based on position of letter d";
    let input: Vec<_> = input.trim().split('\n').map(|x| parse_input(x)).collect();

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn part_1(input: &[Action]) -> String {
    let chars: Vec<_> = "abcdefgh".chars().collect();
    let mut data = VecDeque::with_capacity(chars.len());
    for c in chars { data.push_back(c) };
    for action in input {
        apply_action(action, &mut data);
    }
    data.into_iter().collect()
}

fn part_2(input: &[Action]) -> String {
    let chars: Vec<_> = "abcdefgh".chars().collect();
    for perm in chars.iter().permutations(chars.len()).unique() {
        let mut data = VecDeque::with_capacity(chars.len());
        for c in &perm { data.push_back(**c) };

        for action in input {
            apply_action(action, &mut data);
        }
        let ans: String = data.into_iter().collect();
        if ans == "fbgdceah" {
            return perm.into_iter().collect();
        }
    }
    panic!("Cannot find answer!")
}

fn parse_input(line: &str) -> Action {
    return match line.split(' ').collect::<Vec<_>>()[..] {
        ["swap", "position", a, _, _, b] => { Action::SwapPositions{ a: a.parse().unwrap(), b: b.parse().unwrap() } }
        ["swap", "letter", a, _, _, b] => { Action::SwapLetters{ a: a.chars().next().unwrap(), b: b.chars().next().unwrap() } }
        ["reverse", _, a, _, b] => { return Action::Reverse{a: a.parse().unwrap(), b: b.parse().unwrap() } }
        ["move", _, a, _, _, b] => { return Action::Move{ a: a.parse().unwrap(), b: b.parse().unwrap() } }
        ["rotate", "left", b, _] => { Action::RotateDir{ a: RotateDirection::Left, b: b.parse().unwrap() } }
        ["rotate", "right", b, _] => { Action::RotateDir{ a: RotateDirection::Right, b: b.parse().unwrap() } }
        ["rotate", "based", _, _, _, _, a] => { Action::RotateBased{ a: a.chars().next().unwrap() } }
        _ => { panic!("Missing action!") }
    }
}

fn apply_action(action: &Action, data: &mut VecDeque<char>) {
    match action {
        Action::RotateDir{a, b} => { rotate(a, *b, data) }
        Action::RotateBased{a} => { rotate_based(*a, data) }
        Action::SwapPositions{a, b} => { swap_position(*a, *b, data) }
        Action::SwapLetters{a,b} => { swap_letter(*a, *b, data) }
        Action::Reverse{a, b} => { reverse(*a, *b, data) }
        Action::Move{a, b} => { move_char(*a, *b, data) }
    }
}

fn rotate(dir: &RotateDirection, amount: usize, data: &mut VecDeque<char>) {
    match dir {
        RotateDirection::Left => {
            for _ in 0..amount {
                let x = data.pop_front().unwrap();
                data.push_back(x);
            }
        }
        RotateDirection::Right => {
            for _ in 0..amount {
                let x = data.pop_back().unwrap();
                data.push_front(x);
            }
        }
    }
}

fn rotate_based(letter: char, data: &mut VecDeque<char>) {
    let (mut amount, _) = data.iter().enumerate().find(|(_, &x)| x == letter).unwrap();
    amount += if amount >= 4 {2} else {1};
    rotate(&RotateDirection::Right, amount, data);
}

fn swap_position(pos_a: usize, pos_b: usize, data: &mut VecDeque<char>) {
    data.swap(pos_a, pos_b);
}

fn swap_letter(letter_a: char, letter_b: char, data: &mut VecDeque<char>) {
    let mut x = data.iter().enumerate().filter(|(_, &x)| x == letter_a || x == letter_b);
    let (pos_a, _) = x.next().unwrap();
    let (pos_b, _) = x.next().unwrap();
    swap_position(pos_a, pos_b, data);
}

fn reverse(start: usize, end: usize, data: &mut VecDeque<char>) {
    let mut before = VecDeque::with_capacity(start);
    let mut to_reverse = VecDeque::with_capacity(end-start);

    for _ in 0..start { before.push_back(data.pop_front().unwrap()); }
    for _ in 0..(end-start)+1 { to_reverse.push_back(data.pop_front().unwrap()); }
    for _ in 0..(end-start)+1 { before.push_back(to_reverse.pop_back().unwrap()); }
    for _ in 0..end+1 { data.push_front(before.pop_back().unwrap()); }
}

fn move_char(start: usize, end: usize, data: &mut VecDeque<char>) {
    let mut collector = VecDeque::with_capacity(end);
    for _ in 0..start { collector.push_back(data.pop_front().unwrap()); }
    let c = data.pop_front().unwrap();
    for _ in 0..start { data.push_front(collector.pop_back().unwrap()); }

    for _ in 0..end { collector.push_back(data.pop_front().unwrap()); }
    data.push_front(c);
    for _ in 0..end { data.push_front(collector.pop_back().unwrap()); }
}