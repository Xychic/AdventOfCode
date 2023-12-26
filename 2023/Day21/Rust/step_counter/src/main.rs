use std::{fs, time::Instant};
use step_counter::{parse, part_1, part_2};

fn main() {
    let mut start = Instant::now();
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    println!("Parsed input in {:?}", start.elapsed());

    start = Instant::now();
    println!("Part 1: {}, took {:?}", part_1(&input, 64), start.elapsed());
    start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2(&input, 26_501_365),
        start.elapsed()
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_1_INPUT: &str = "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........";
    const TEST_1_ANSWER: usize = 16;
    const TEST_2_INPUT: &str = TEST_1_INPUT;
    const TEST_2_ANSWER: usize = 1594;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(TEST_1_INPUT), 6), TEST_1_ANSWER);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(TEST_2_INPUT), 5000), TEST_2_ANSWER);
    }
}
