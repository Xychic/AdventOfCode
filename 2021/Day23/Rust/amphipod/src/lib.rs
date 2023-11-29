type State<const N: usize> = ([u8; 11], [[u8; N]; 4]);
type Input = State<2>;

pub fn parse(input: &str) -> Input {
    let x: Vec<_> = input
        .trim()
        .lines()
        .skip(2)
        .take(2)
        .map(str::as_bytes)
        .collect();
    let (line_1, line_2) = (x[0], x[1]);

    (
        [b'.'; 11],
        [
            [line_1[3], line_2[3]],
            [line_1[5], line_2[5]],
            [line_1[7], line_2[7]],
            [line_1[9], line_2[9]],
        ],
    )
}

pub fn part_1(input: &Input) -> usize {
    get_lowest_cost(*input)
}

pub fn part_2(&input: &Input) -> usize {
    let (_, [[a1, a2], [b1, b2], [c1, c2], [d1, d2]]) = input;
    let part_2_input = (
        [b'.'; 11],
        [
            [a1, b'D', b'D', a2],
            [b1, b'C', b'B', b2],
            [c1, b'B', b'A', c2],
            [d1, b'A', b'C', d2],
        ],
    );
    get_lowest_cost(part_2_input)
}

use std::{
    cmp::Reverse,
    collections::{HashMap, HashSet},
};

use priority_queue::PriorityQueue;

fn make_move<const N: usize>(
    (mut corridor, mut rooms): State<N>,
    c: usize,
    room: usize,
    depth: usize,
) -> (State<N>, usize) {
    let piece = if corridor[c] == b'.' {
        rooms[room][depth]
    } else {
        corridor[c]
    } - b'A';
    let c0 = [2, 4, 6, 8][room];
    let cost =
        (depth + if c0 > c { c0 - c } else { c - c0 } + 1) * [1, 10, 100, 1000][piece as usize];
    std::mem::swap(&mut corridor[c], &mut rooms[room][depth]);
    ((corridor, rooms), cost)
}

fn get_possible_moves<const N: usize>((corridor, rooms): State<N>) -> Vec<(State<N>, usize)> {
    let mut moves = Vec::with_capacity(44);
    for c in 0..corridor.len() {
        if corridor[c] == b'.' {
            continue;
        }

        let target_room = (corridor[c] - b'A') as usize;
        let c0 = [2, 4, 6, 8][target_room];
        let (r0, r1) = if c > c0 { (c0, c) } else { (c + 1, c0 + 1) };
        if (r0..r1).any(|i| corridor[i] != b'.') {
            continue;
        }


        
        let i = match (0..N).take_while(|&i| rooms[target_room][i] == b'.').last() {
            Some(i) => i,
            None => continue,
        };
        if ((i + 1)..N).any(|d| rooms[target_room][d] != corridor[c]) {
            continue;
        }
        moves.push(make_move((corridor, rooms), c, target_room, i));
    }

    for room in 0..4 {
        let i = match (0..N).find(|&i| rooms[room][i] != b'.') {
            Some(i) => i,
            _ => continue,
        };
        let c0 = [2, 4, 6, 8][room];
        let valid_moves = (c0..corridor.len())
            .take_while(|&c| corridor[c] == b'.')
            .chain((0..c0).rev().take_while(|&c| corridor[c] == b'.'))
            .filter(|c| ![2, 4, 6, 8].contains(c))
            .map(|c| make_move((corridor, rooms), c, room, i));
        moves.extend(valid_moves);
    }
    moves
}

fn get_lowest_cost<const N: usize>(state: State<N>) -> usize {
    let mut unexplored_states = PriorityQueue::new();
    let mut explored_states = HashSet::new();
    let mut cost_to_state = HashMap::new();

    cost_to_state.insert(state, 0_usize);
    unexplored_states.push(state, Reverse(0));

    while let Some((current_state, Reverse(current_cost))) = unexplored_states.pop() {
        explored_states.insert(current_state);

        if current_state.1 == [[b'A'; N], [b'B'; N], [b'C'; N], [b'D'; N]] {
            return current_cost;
        }

        for (next_state, move_cost) in get_possible_moves(current_state) {
            if explored_states.contains(&next_state) {
                continue;
            }
            let new_cost = current_cost + move_cost;

            if new_cost < *cost_to_state.get(&next_state).unwrap_or(&usize::MAX) {
                cost_to_state.insert(next_state, new_cost);
                unexplored_states.push(next_state, Reverse(new_cost));
            }
        }
    }

    unreachable!()
}
