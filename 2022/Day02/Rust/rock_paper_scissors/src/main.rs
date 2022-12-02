use std::{fs, time::Instant};

type Input<'a> = Vec<(usize, usize)>;
fn main() {
    let start = Instant::now();
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    println!("Parsed input in {:?}", Instant::now() - start);

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
        .split('\n')
        .map(|l| {
            let (a, b) = l.split_once(' ').unwrap();
            (
                match a {
                    "A" => 0,
                    "B" => 1,
                    "C" => 2,
                    _ => unreachable!(),
                },
                match b {
                    "X" => 0,
                    "Y" => 1,
                    "Z" => 2,
                    _ => unreachable!(),
                },
            )
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    input
        .iter()
        .map(|&(a, b)| {
            if a == b {
                4 + b
            } else if b == (a + 1) % 3 {
                7 + b
            } else {
                b + 1
            }
        })
        .sum()
}

fn part_2(input: &Input) -> usize {
    input
        .iter()
        .map(|&(a, b)| match b {
            0 => 1 + ((a + 2) % 3),
            1 => 4 + a,
            2 => 7 + ((a + 1) % 3),
            _ => unreachable!(),
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "A Y
B X
C Z";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 15);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 12);
    }
}
