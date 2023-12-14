use std::collections::HashMap;

type Input<'a> = Vec<Vec<char>>;

/// Parser for 2023 Day 14 (`parabolic_reflector_dish`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input.trim().lines().map(|l| l.chars().collect()).collect()
}

fn slide(data: &mut Input, dir: usize) {
    match dir {
        0 => {
            for x in 0..data[0].len() {
                loop {
                    let mut changes = false;

                    for y in 1..data.len() {
                        if data[y][x] == 'O' && data[y - 1][x] == '.' {
                            data[y][x] = '.';
                            data[y - 1][x] = 'O';
                            changes = true;
                        }
                    }

                    if !changes {
                        break;
                    }
                }
            }
        }
        1 => {
            for y in 0..data.len() {
                loop {
                    let mut changes = false;

                    for x in 1..data[0].len() {
                        if data[y][x] == 'O' && data[y][x - 1] == '.' {
                            data[y][x] = '.';
                            data[y][x - 1] = 'O';
                            changes = true;
                        }
                    }
                    if !changes {
                        break;
                    }
                }
            }
        }
        2 => {
            for x in 0..data[0].len() {
                loop {
                    let mut changes = false;

                    for y in (0..data.len() - 1).rev() {
                        if data[y][x] == 'O' && data[y + 1][x] == '.' {
                            data[y][x] = '.';
                            data[y + 1][x] = 'O';
                            changes = true;
                        }
                    }

                    if !changes {
                        break;
                    }
                }
            }
        }
        3 => {
            for y in 0..data.len() {
                loop {
                    let mut changes = false;

                    for x in (0..data[0].len() - 1).rev() {
                        if data[y][x] == 'O' && data[y][x + 1] == '.' {
                            data[y][x] = '.';
                            data[y][x + 1] = 'O';
                            changes = true;
                        }
                    }

                    if !changes {
                        break;
                    }
                }
            }
        }
        _ => unreachable!(),
    }
}

fn get_load(data: &Input) -> usize {
    data.iter()
        .rev()
        .enumerate()
        .map(|(i, row)| row.iter().filter(|&&c| c == 'O').count() * (i + 1))
        .sum()
}

/// Solver for part 1 of 2023 Day 14 (`parabolic_reflector_dish`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut data = input.to_owned();
    slide(&mut data, 0);
    get_load(&data)
}

/// Solver for part 2 of 2023 Day 14 (`parabolic_reflector_dish`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut data = input.to_owned();
    let mut seen = HashMap::new();

    let target_cycles = 1_000_000_000;
    let mut cycles = 0;
    while cycles < target_cycles {
        cycles += 1;
        for dir in 0..4 {
            slide(&mut data, dir);
        }
        let key = format!("{data:?}");
        if let Some(x) = seen.get(&key) {
            let cycle_len = cycles - x;
            let full_cycles_left = (target_cycles - cycles) / cycle_len;
            cycles += full_cycles_left * cycle_len;
        }
        seen.insert(key, cycles);
    }

    get_load(&data)
}
