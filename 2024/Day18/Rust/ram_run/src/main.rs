use ram_run::{parse, part_1, part_2};
use std::{fs, time::Instant};

fn main() {
    let mut start = Instant::now();
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    println!("Parsed input in {:?}", start.elapsed());

    start = Instant::now();
    println!(
        "Part 1: {:?}, took {:?}",
        part_1::<70, 70, 1024>(&input),
        start.elapsed()
    );
    start = Instant::now();
    println!(
        "Part 2: {:?}, took {:?}",
        part_2::<70, 70, 1024>(&input),
        start.elapsed()
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_1_INPUT: &str = "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0";
    const TEST_1_ANSWER: usize = 22;
    const TEST_2_INPUT: &str = TEST_1_INPUT;
    const TEST_2_ANSWER: &str = "6,1";

    #[test]
    fn test_part_1() {
        assert_eq!(part_1::<6, 6, 12>(&parse(TEST_1_INPUT)), TEST_1_ANSWER);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2::<6, 6, 12>(&parse(TEST_2_INPUT)), TEST_2_ANSWER);
    }
}
