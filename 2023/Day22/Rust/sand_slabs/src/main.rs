use sand_slabs::{parse, part_1, part_2};
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
    const TEST_1_INPUT: &str = "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9";
    const TEST_1_ANSWER: usize = 5;
    const TEST_2_INPUT: &str = TEST_1_INPUT;
    const TEST_2_ANSWER: usize = 7;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(TEST_1_INPUT)), TEST_1_ANSWER);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(TEST_2_INPUT)), TEST_2_ANSWER);
    }
}
