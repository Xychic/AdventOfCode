use std::fs;

type Input = Vec<Vec<char>>;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");

    println!("Part 1: {}", part_1(&raw_input));
    println!("Part 2: {}", part_2(&raw_input));
}

fn parse(input: &str) -> Input {
    input
        .trim()
        .split('\n')
        .map(|x| x.chars().collect())
        .collect()
}

fn part_1(input: &str) -> usize {
    let dirs = [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];
    let mut current = parse(input);
    let mut next;
    for _ in 0..10 {
        next = Vec::with_capacity(current.len());
        for (y, row) in current.iter().enumerate() {
            let mut new_row = Vec::with_capacity(row.len());
            for (x, c) in row.iter().enumerate() {
                let mut l = 0;
                let mut t = 0;
                for (dx, dy) in dirs {
                    let x2 = (x as isize) + dx;
                    let y2 = (y as isize) + dy;
                    if 0 <= x2 && x2 < row.len() as isize && 0 <= y2 && y2 < current.len() as isize
                    {
                        let x2 = x2 as usize;
                        let y2 = y2 as usize;
                        match current[y2][x2] {
                            '|' => t += 1,
                            '#' => l += 1,
                            _ => (),
                        }
                    }
                }
                if c == &'.' && t >= 3 {
                    new_row.push('|');
                } else if c == &'|' && l >= 3 {
                    new_row.push('#');
                } else if c == &'#' && (l == 0 || t == 0) {
                    new_row.push('.');
                } else {
                    new_row.push(*c);
                }
            }
            next.push(new_row);
        }
        current = next;
    }
    let wooded: usize = current
        .iter()
        .map(|x| x.iter().filter(|&c| c == &'|').count())
        .sum();
    let lumberyards: usize = current
        .iter()
        .map(|x| x.iter().filter(|&c| c == &'#').count())
        .sum();
    wooded * lumberyards
}

fn part_2(input: &str) -> usize {
    let dirs = [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, -1),
        (0, 1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];
    let mut current = parse(input);
    let mut next;
    let mut seen = Vec::with_capacity(1000);
    let mut cycle = 0;
    while cycle == 0 {
        next = Vec::with_capacity(current.len());
        for (y, row) in current.iter().enumerate() {
            let mut new_row = Vec::with_capacity(row.len());
            for (x, c) in row.iter().enumerate() {
                let mut l = 0;
                let mut t = 0;
                for (dx, dy) in dirs {
                    let x2 = (x as isize) + dx;
                    let y2 = (y as isize) + dy;
                    if 0 <= x2 && x2 < row.len() as isize && 0 <= y2 && y2 < current.len() as isize
                    {
                        let x2 = x2 as usize;
                        let y2 = y2 as usize;
                        match current[y2][x2] {
                            '|' => t += 1,
                            '#' => l += 1,
                            _ => (),
                        }
                    }
                }
                if c == &'.' && t >= 3 {
                    new_row.push('|');
                } else if c == &'|' && l >= 3 {
                    new_row.push('#');
                } else if c == &'#' && (l == 0 || t == 0) {
                    new_row.push('.');
                } else {
                    new_row.push(*c);
                }
            }
            next.push(new_row);
        }
        current = next;
        let wooded: usize = current
            .iter()
            .map(|x| x.iter().filter(|&c| c == &'|').count())
            .sum();
        let lumberyards: usize = current
            .iter()
            .map(|x| x.iter().filter(|&c| c == &'#').count())
            .sum();
        let val = wooded * lumberyards;
        for i in (0..seen.len()).rev() {
            if seen[i] == val {
                cycle = seen.len() - i;
                for d in 1..=cycle {
                    if seen[i - d] != seen[seen.len() - d] {
                        cycle = 0;
                        break;
                    }
                }
                break;
            }
        }
        seen.push(val);
    }
    let t_index = 1000000000 % cycle;
    let c_index = seen.len() % cycle;
    let ans_delta = cycle - (c_index as isize - t_index as isize).abs() as usize;
    seen[seen.len() - 1 - ans_delta]
}

fn _show(grid: &[Vec<char>]) {
    for row in grid {
        for c in row {
            print!("{}", c);
        }
        println!();
    }
    println!();
}
