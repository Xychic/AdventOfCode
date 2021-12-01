use std::{
    cmp,
    collections::{HashMap, HashSet, VecDeque},
    fs,
};

use regex::Regex;

type Input = HashMap<(usize, usize), char>;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");

    let finished = flow_water(&raw_input);
    println!("Part 1: {}", part_1(&finished));
    println!("Part 2: {}", part_2(&finished));
}

fn parse(input: &str) -> Input {
    let mut grid = HashMap::new();
    let mut max_x = 0;
    let mut max_y = 0;
    for line in input.trim().split('\n') {
        let re = Regex::new(r"([xy])=([0-9]+), ([xy])=([0-9]+)..([0-9]+)").unwrap();
        let caps = re.captures(line).unwrap();
        let first = &caps[1];
        let num1: usize = caps[2].parse().unwrap();
        let lower: usize = caps[4].parse().unwrap();
        let upper: usize = caps[5].parse().unwrap();

        match first {
            "x" => {
                for y in lower..=upper {
                    max_y = cmp::max(max_y, y);
                    grid.insert((num1, y), '#');
                }
                max_x = cmp::max(max_x, num1);
            }
            "y" => {
                for x in lower..=upper {
                    grid.insert((x, num1), '#');
                    max_x = cmp::max(max_x, x);
                }
                max_y = cmp::max(max_y, num1);
            }
            _ => panic!(),
        }
    }
    grid
}

fn part_1(input: &Input) -> usize {
    input.values().filter(|&c| c == &'w' || c == &'o').count()
}

fn part_2(input: &Input) -> usize {
    let mut retained = HashSet::new();
    for (x, y) in input.keys().clone() {
        if retained.contains(&(*x, *y)) {
            continue;
        }
        if input.get(&(*x, *y)).unwrap() == &'w' {
            let mut row = HashSet::new();
            let mut valid = true;
            row.insert((*x, *y));

            let mut x2 = x + 1;
            while let Some(c) = input.get(&(x2, *y)) {
                match c {
                    'w' => {
                        row.insert((x2, *y));
                        x2 += 1;
                    }
                    '#' => break,
                    _ => {
                        valid = false;
                        break;
                    }
                }
            }
            if valid {
                let mut x2 = x - 1;
                while let Some(c) = input.get(&(x2, *y)) {
                    match c {
                        'w' => {
                            row.insert((x2, *y));
                            x2 -= 1;
                        }
                        '#' => break,
                        _ => {
                            valid = false;
                            break;
                        }
                    }
                }
            }

            if valid {
                for key in row {
                    retained.insert(key);
                }
            }
        }
    }

    retained.len()
}

fn flow_water(input: &str) -> Input {
    let mut grid = parse(input);
    let mut queue = VecDeque::new();
    let mut max_y = 0;
    let mut min_y = usize::MAX;
    for (_, y) in grid.keys() {
        max_y = cmp::max(max_y, *y);
        min_y = cmp::min(min_y, *y);
    }

    queue.push_back((500, min_y));
    grid.insert((500, min_y), 'w');

    while let Some((x, y)) = queue.pop_back() {
        if !grid.contains_key(&(x, y + 1)) {
            if y < max_y {
                grid.insert((x, y), 'w');
                queue.push_back((x, y));
                queue.push_back((x, y + 1));
            } else {
                grid.insert((x, y), 'o');
            }
            continue;
        }
        if grid.get(&(x, y + 1)).unwrap() == &'o' {
            grid.insert((x, y), 'o');
            continue;
        }

        let mut in_row = false;
        let mut x2 = x + 1;
        while let Some(c) = grid.get(&(x2, y)) {
            match c {
                'o' => {
                    in_row = true;
                    break;
                }
                'w' => x2 += 1,
                _ => break,
            }
        }
        if !in_row {}
        x2 = x - 1;
        while let Some(c) = grid.get(&(x2, y)) {
            match c {
                'o' => {
                    in_row = true;
                    break;
                }
                'w' => x2 -= 1,
                _ => break,
            }
        }
        grid.insert((x, y), if in_row { 'o' } else { 'w' });

        let contains_left = grid.contains_key(&(x - 1, y));
        let contains_right = grid.contains_key(&(x + 1, y));

        if contains_left && contains_right {
            continue;
        }

        queue.push_back((x, y));

        if !contains_left {
            queue.push_back((x - 1, y));
            grid.insert((x - 1, y), 'w');
        }
        if !contains_right {
            queue.push_back((x + 1, y));
            grid.insert((x + 1, y), 'w');
        }
    }
    grid
}

fn _draw_grid(grid: &Input) {
    let mut max_x = 0;
    let mut min_x = usize::MAX;
    let mut max_y = 0;
    let mut min_y = usize::MAX;

    for (x, y) in grid.keys() {
        max_x = cmp::max(max_x, *x);
        min_x = cmp::min(min_x, *x);
        max_y = cmp::max(max_y, *y);
        min_y = cmp::min(min_y, *y);
    }

    for y in min_y..=max_y {
        for x in min_x..=max_x {
            print!("{}", grid.get(&(x, y)).unwrap_or(&' '));
        }
        println!();
    }
    println!();
}
