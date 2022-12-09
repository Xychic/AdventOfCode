use rope_bridge::*;
use std::{fs, time::Instant};

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
    const TEST_1_INPUT: &str = "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2";
    const TEST_1_ANSWER: usize = 13;
    const TEST_2_INPUT: &str = "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20";
    const TEST_2_ANSWER: usize = 36;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_1_INPUT)), TEST_1_ANSWER);
    }

    #[test]
    fn test_part_2() {
        if part_1(&parse(&TEST_1_INPUT)) == TEST_1_ANSWER {
            assert_eq!(part_2(&parse(&TEST_2_INPUT)), TEST_2_ANSWER);
        }
    }
}
