use std::{fs, time::Instant};
use unstable_diffusion::*;

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

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_1_INPUT: &str = "....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..";

    const TEST_1_ANSWER: usize = 110;
    const TEST_2_INPUT: &str = TEST_1_INPUT;
    const TEST_2_ANSWER: usize = 20;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_1_INPUT)), TEST_1_ANSWER);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_2_INPUT)), TEST_2_ANSWER);
    }
}
