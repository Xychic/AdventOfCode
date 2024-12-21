use keypad_conundrum::{parse, part_1, part_2};
use std::{fs, time::Instant};

fn main() {
    let mut start = Instant::now();
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    println!("Parsed input in {:?}", start.elapsed());

    start = Instant::now();
    println!("Part 1: {:?}, took {:?}", part_1(&input), start.elapsed());
    start = Instant::now();
    println!("Part 2: {:?}, took {:?}", part_2(&input), start.elapsed());
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_1_INPUT: &str = "029A
980A
179A
456A
379A";
    const TEST_1_ANSWER: usize = 126_384;
    const TEST_2_INPUT: &str = TEST_1_INPUT;
    const TEST_2_ANSWER: usize = 154_115_708_116_294;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(TEST_1_INPUT)), TEST_1_ANSWER);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(TEST_2_INPUT)), TEST_2_ANSWER);
    }
}
