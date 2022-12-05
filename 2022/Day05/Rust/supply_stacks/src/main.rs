use std::{fs, time::Instant};
use supply_stacks::*;

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
    const TEST_1_INPUT: &str = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2";
    const TEST_1_ANSWER: &str = "CMZ";
    const TEST_2_INPUT: &str = TEST_1_INPUT;
    const TEST_2_ANSWER: &str = "MCD";

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
