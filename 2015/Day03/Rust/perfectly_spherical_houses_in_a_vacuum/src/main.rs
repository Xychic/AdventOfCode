use std::{collections::HashSet, fs, time::Instant};

type Input<'a> = Vec<(isize, isize)>;

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
    input
        .trim()
        .chars()
        .map(|c| match c {
            '^' => (0, 1),
            '>' => (1, 0),
            'v' => (0, -1),
            '<' => (-1, 0),
            _otherwise => unreachable!(),
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    let mut x = 0;
    let mut y = 0;
    let mut seen = HashSet::with_capacity(input.len());
    seen.insert((x, y));
    for (dx, dy) in input {
        x += dx;
        y += dy;
        seen.insert((x, y));
    }
    seen.len()
}

fn part_2(input: &Input) -> usize {
    let mut x1 = 0;
    let mut y1 = 0;
    let mut x2 = 0;
    let mut y2 = 0;
    let mut santa = true;
    let mut seen = HashSet::with_capacity(input.len());
    seen.insert((x1, y1));
    for (dx, dy) in input {
        if santa {
            x1 += dx;
            y1 += dy;
            seen.insert((x1, y1));
        } else {
            x2 += dx;
            y2 += dy;
            seen.insert((x2, y2));
        }
        santa = !santa;
    }
    seen.len()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "^>v<";
    const TEST_INPUT_2: &str = "^v^v^v^v^v";

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 4);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 11);
    }
}
