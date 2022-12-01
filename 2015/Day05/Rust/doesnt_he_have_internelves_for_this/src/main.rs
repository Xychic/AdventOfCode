use std::{fs, time::Instant};

type Input<'a> = Vec<&'a str>;

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
    input.trim().split('\n').collect()
}

fn is_nice(string: &str) -> bool {
    let vowel = ['a', 'e', 'i', 'o', 'u'];
    let vowel_count = string.chars().filter(|c| vowel.contains(c)).count() >= 3;
    let double_letter = string
        .chars()
        .zip(string.chars().skip(1))
        .any(|(a, b)| a == b);

    let banned = !["ab", "cd", "pq", "xy"].iter().any(|x| string.contains(x));
    vowel_count && double_letter && banned
}

fn is_nice_2(input: &str) -> bool {
    let valid_pairs = (0..(input.len() - 1)).any(|i| input.matches(&input[i..i + 2]).count() >= 2);
    let valid_repeat = (0..(input.len() - 2)).any(|i| input[i..i + 1] == input[i + 2..i + 3]);
    valid_pairs && valid_repeat
}

fn part_1(input: &Input) -> usize {
    input.iter().filter(|x| is_nice(x)).count()
}

fn part_2(input: &Input) -> usize {
    input.iter().filter(|x| is_nice_2(x)).count()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "ugknbfddgicrmopn";
    const TEST_INPUT_2: &str = "qjhvhtzxzqqjkmpb
xxyxx
uurcxstgmygtbstg
ieodomkazucvgmuy
aaa";

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 1);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 2);
    }
}
