use std::collections::{HashMap, HashSet, VecDeque};

type Input<'a> = (HashMap<Point, char>, Vec<usize>, Point);
type Point = (usize, usize);

/// Parser for 2024 Day 15 (`warehouse_woes`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let (map_str, instructions) = input.trim().split_once("\n\n").unwrap();
    let mut map = HashMap::new();
    let mut robot = None;
    for (y, line) in map_str.lines().enumerate() {
        for (x, c) in line.char_indices() {
            map.insert(
                (x, y),
                match c {
                    '@' => {
                        robot = Some((x, y));
                        '.'
                    }
                    x => x,
                },
            );
        }
    }

    (
        map,
        instructions
            .chars()
            .filter_map(|c| match c {
                '^' => Some(0),
                '>' => Some(1),
                'v' => Some(2),
                '<' => Some(3),
                _ => None,
            })
            .collect(),
        robot.unwrap(),
    )
}

/// Solver for part 1 of 2024 Day 15 (`warehouse_woes`)
///
/// # Panics
#[must_use]
pub fn part_1((map, instructions, robot): &Input) -> usize {
    let (mut x, mut y) = robot;
    let mut map = map.to_owned();
    for &step in instructions {
        let new_pos = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)][step];
        match map.get(&new_pos) {
            Some('.') => {
                (x, y) = new_pos;
            }
            Some('#') => continue,
            Some('O') => {
                let mut test_pos @ (mut test_x, mut test_y) = new_pos;
                while map.get(&test_pos) == Some(&'O') {
                    test_pos = [
                        (test_x, test_y - 1),
                        (test_x + 1, test_y),
                        (test_x, test_y + 1),
                        (test_x - 1, test_y),
                    ][step];
                    (test_x, test_y) = test_pos;
                }
                match map.get(&test_pos) {
                    Some('#') => continue,
                    Some('.') => {
                        map.insert(test_pos, 'O');
                        map.insert(new_pos, '.');
                        (x, y) = new_pos;
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    map.iter()
        .filter_map(|((x, y), c)| match c {
            'O' => Some(100 * y + x),
            _ => None,
        })
        .sum()
}

/// Solver for part 2 of 2024 Day 15 (`warehouse_woes`)
///
/// # Panics
#[must_use]
pub fn part_2((old_map, instructions, robot): &Input) -> usize {
    let mut map = HashMap::new();
    let mut max_x = 0;
    let mut max_y = 0;
    for (&(x, y), c) in old_map {
        if c == &'O' {
            map.insert((x * 2, y), '[');
            map.insert((x * 2 + 1, y), ']');
        } else {
            map.insert((x * 2, y), *c);
            map.insert((x * 2 + 1, y), *c);
        }
        max_x = max_x.max(x * 2 + 1);
        max_y = max_y.max(y);
    }

    let (mut x, mut y) = (robot.0 * 2, robot.1);
    'steps: for &step in instructions {
        let new_pos = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)][step];
        match map.get(&new_pos) {
            Some('.') => {
                (x, y) = new_pos;
            }
            Some('#') => continue,
            Some('[' | ']') => {
                let mut queue = VecDeque::new();
                let mut seen = HashSet::new();
                queue.push_back((x, y));
                while let Some(box_pos @ (box_x, box_y)) = queue.pop_front() {
                    if seen.contains(&box_pos) {
                        continue;
                    }
                    seen.insert(box_pos);
                    let test_pos @ (test_x, test_y) = [
                        (box_x, box_y - 1),
                        (box_x + 1, box_y),
                        (box_x, box_y + 1),
                        (box_x - 1, box_y),
                    ][step];
                    match map.get(&test_pos) {
                        Some('#') => continue 'steps,
                        Some('[') => {
                            queue.push_back((test_x, test_y));
                            queue.push_back((test_x + 1, test_y));
                        }
                        Some(']') => {
                            queue.push_back((test_x, test_y));
                            queue.push_back((test_x - 1, test_y));
                        }
                        _ => continue,
                    }
                }
                // println!("{seen:?}");
                // unreachable!();
                let mut to_remove: Vec<_> = seen.iter().collect();
                while !to_remove.is_empty() {
                    for i in 0..to_remove.len() {
                        let box_pos @ &(box_x, box_y) = to_remove[i];
                        let new_pos = [
                            (box_x, box_y - 1),
                            (box_x + 1, box_y),
                            (box_x, box_y + 1),
                            (box_x - 1, box_y),
                        ][step];
                        if map.get(&new_pos) == Some(&'.') {
                            map.insert(new_pos, *map.get(box_pos).unwrap());
                            map.insert(*box_pos, '.');
                            to_remove.remove(i);
                            break;
                        }
                    }
                }
                (x, y) = new_pos;
            }
            _ => unreachable!(),
        }
        // for map_y in 0..=max_y {
        //     for map_x in 0..=max_x {
        //         if (map_x, map_y) == (x, y) {
        //             print!("@");
        //         } else {
        //             print!(
        //                 "{}",
        //                 match map.get(&(map_x, map_y)).unwrap() {
        //                     '.' => ' ',
        //                     x => *x,
        //                 }
        //             );
        //         }
        //     }
        //     println!();
        // }
        // std::thread::sleep(std::time::Duration::from_millis(10));
    }

    map.iter()
        .filter_map(|((x, y), c)| match c {
            '[' => Some(100 * y + x),
            _ => None,
        })
        .sum()
}
