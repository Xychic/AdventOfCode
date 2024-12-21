use std::collections::{HashMap, VecDeque};

type Input<'a> = Vec<&'a str>;
type Point = (usize, usize);

/// Parser for 2024 Day 21 (`keypad_conundrum`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input.trim().lines().collect()
}

/// Solver for part 1 of 2024 Day 21 (`keypad_conundrum`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    solve::<2>(input)
}

/// Solver for part 2 of 2024 Day 21 (`keypad_conundrum`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    solve::<25>(input)
}

fn solve<const D: usize>(input: &Input) -> usize {
    let keycode_map = create_maps(&HashMap::from([
        ((0, 0), '7'),
        ((1, 0), '8'),
        ((2, 0), '9'),
        ((0, 1), '4'),
        ((1, 1), '5'),
        ((2, 1), '6'),
        ((0, 2), '1'),
        ((1, 2), '2'),
        ((2, 2), '3'),
        ((1, 3), '0'),
        ((2, 3), 'A'),
    ]));

    let button_map = create_maps(&HashMap::from([
        ((1, 0), '^'),
        ((2, 0), 'A'),
        ((0, 1), '<'),
        ((1, 1), 'v'),
        ((2, 1), '>'),
    ]));

    let mut seen = HashMap::new();
    input
        .iter()
        .map(|code| {
            code[..code.len() - 1].parse::<usize>().unwrap()
                * create_permutations(
                    &std::iter::once('A')
                        .chain(code.chars())
                        .zip(code.chars())
                        .map(|p| keycode_map.get(&p).unwrap())
                        .collect::<Vec<_>>(),
                )
                .iter()
                .map(|p| get_step_count(p, D, &button_map, &mut seen))
                .min()
                .unwrap()
        })
        .sum()
}

fn create_maps(grid: &HashMap<Point, char>) -> HashMap<(char, char), Vec<String>> {
    let mut map = HashMap::new();
    for &start in grid.values() {
        for &end in grid.values() {
            map.insert((start, end), get_all_paths(start, end, grid));
        }
    }
    map
}

fn get_all_paths(start: char, end: char, grid: &HashMap<Point, char>) -> Vec<String> {
    let mut queue = VecDeque::new();
    queue.push_back((
        *grid.iter().find(|(_, &c)| c == start).unwrap().0,
        String::new(),
    ));

    let mut possible = Vec::new();
    let mut len = None;

    while let Some((pos @ (p_x, p_y), path)) = queue.pop_front() {
        if grid.get(&pos) == Some(&end) {
            if len.is_none() || Some(path.len()) == len {
                len = Some(path.len());
                possible.push(path + "A");
            }
            continue;
        }
        if let Some(d) = len {
            if path.len() > d {
                continue;
            }
        }
        for (new_pos, dir) in [
            ((p_x, p_y - 1), "^"),
            ((p_x + 1, p_y), ">"),
            ((p_x, p_y + 1), "v"),
            ((p_x - 1, p_y), "<"),
        ] {
            if grid.contains_key(&new_pos) {
                queue.push_back((new_pos, path.clone() + dir));
            }
        }
    }
    possible
}

fn create_permutations(possible: &[&Vec<String>]) -> Vec<String> {
    if possible.len() == 1 {
        return possible[0].clone();
    }
    let mut ans = Vec::new();
    for a in possible[0] {
        for b in create_permutations(&possible[1..]) {
            ans.push(a.clone() + &b);
        }
    }

    ans
}

fn get_step_count(
    target: &str,
    steps: usize,
    paths: &HashMap<(char, char), Vec<String>>,
    seen: &mut HashMap<(String, usize), usize>,
) -> usize {
    if let Some(&ans) = seen.get(&(target.to_string(), steps)) {
        return ans;
    }
    let ans = std::iter::once('A')
        .chain(target.chars())
        .zip(target.chars())
        .map(|p| {
            paths
                .get(&p)
                .unwrap()
                .iter()
                .map(|xs| {
                    if steps == 1 {
                        xs.len()
                    } else {
                        get_step_count(xs, steps - 1, paths, seen)
                    }
                })
                .min()
                .unwrap()
        })
        .sum();
    seen.insert((target.to_string(), steps), ans);
    ans
}
