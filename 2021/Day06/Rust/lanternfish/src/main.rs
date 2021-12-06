use std::{collections::HashMap, fs, time::Instant};

type Input = Vec<usize>;

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
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect()
}

fn part_1(input: &Input) -> usize {
    count_fish(input, 80)
}

fn part_2(input: &Input) -> usize {
    count_fish(input, 256)
}

fn count_fish(fish: &Input, days: usize) -> usize {
    let mut today = HashMap::with_capacity(9);
    for f in fish {
        *today.entry(*f).or_insert(0) += 1;
    }

    for _ in 0..days {
        let mut next_day = HashMap::with_capacity(9);
        for (timer, count) in today {
            if timer == 0 {
                *next_day.entry(6).or_insert(0) += count;
                *next_day.entry(8).or_insert(0) += count;
            } else {
                *next_day.entry(timer - 1).or_insert(0) += count;
            }
        }
        today = next_day;
    }

    today.values().sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        let test_input = "3,4,3,1,2";
        let test_answer = 5934;
        assert_eq!(part_1(&parse(&test_input)), test_answer);
    }

    #[test]
    fn test_part_2() {
        let test_input = "3,4,3,1,2";
        let test_answer = 26984457539;
        assert_eq!(part_2(&parse(&test_input)), test_answer);
    }
}
