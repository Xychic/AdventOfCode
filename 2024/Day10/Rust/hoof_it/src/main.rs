use hoof_it::{parse, part_1, part_2};
use std::{fs, time::Instant};

fn main() {
    let mut start = Instant::now();
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    println!("Parsed input in {:?}", start.elapsed());

    start = Instant::now();
    println!("Part 1: {}, took {:?}", part_1(&input), start.elapsed());
    start = Instant::now();
    println!("Part 2: {}, took {:?}", part_2(&input), start.elapsed());
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_1_INPUT: &str = "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732";
    const TEST_1_ANSWER: usize = 36;
    const TEST_2_INPUT: &str = TEST_1_INPUT;
    const TEST_2_ANSWER: usize = 81;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(TEST_1_INPUT)), TEST_1_ANSWER);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(TEST_2_INPUT)), TEST_2_ANSWER);
    }
}