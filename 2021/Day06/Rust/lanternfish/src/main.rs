use std::{collections::VecDeque, fs, time::Instant};

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
    let mut queue = VecDeque::from(vec![0; 9]);
    for f in fish {
        queue[*f] += 1;
    }

    for _ in 0..days {
        let c = queue.pop_front().unwrap();
        queue[6] += c;
        queue.push_back(c);
    }

    queue.iter().sum()
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
