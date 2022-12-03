use std::{fs, time::Instant};

type Input<'a> = Vec<&'a str>;

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
    input.trim().split('\n').collect()
}

fn part_1(input: &Input) -> usize {
    input
        .iter()
        .map(|l| {
            let len = l.len() / 2;
            let (a, b) = (&l[..len], &l[len..]);
            let same = a.chars().filter(|&c| b.contains(c)).next().unwrap();
            if same.is_uppercase() {
                27 + same as usize - 'A' as usize
            } else {
                1 + same as usize - 'a' as usize
            }
        })
        .sum()
}

fn part_2(input: &Input) -> usize {
    input
        .chunks(3)
        .map(|x| {
            let (a, b, c) = (x[0], x[1], x[2]);
            let badge = a
                .chars()
                .filter(|&char| b.contains(char) && c.contains(char))
                .next()
                .unwrap();
            if badge.is_uppercase() {
                27 + badge as usize - 'A' as usize
            } else {
                1 + badge as usize - 'a' as usize
            }
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 157);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 70);
    }
}
