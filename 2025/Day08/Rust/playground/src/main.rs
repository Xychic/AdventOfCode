use playground::{parse, part_1, part_2};
use std::{fs, time::Instant};

fn main() {
    let mut start = Instant::now();
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    println!("Parsed input in {:?}", start.elapsed());

    start = Instant::now();
    println!(
        "Part 1: {:?}, took {:?}",
        part_1::<1000>(&input),
        start.elapsed()
    );
    start = Instant::now();
    println!("Part 2: {:?}, took {:?}", part_2(&input), start.elapsed());
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_1_INPUT: &str = "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689";
    const TEST_1_ANSWER: usize = 40;
    const TEST_2_INPUT: &str = TEST_1_INPUT;
    const TEST_2_ANSWER: usize = 25272;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1::<10>(&parse(TEST_1_INPUT)), TEST_1_ANSWER);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(TEST_2_INPUT)), TEST_2_ANSWER);
    }
}
