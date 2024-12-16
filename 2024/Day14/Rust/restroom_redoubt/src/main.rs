use restroom_redoubt::{parse, part_1, part_2};
use std::{fs, time::Instant};

fn main() {
    let mut start = Instant::now();
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    println!("Parsed input in {:?}", start.elapsed());

    start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1::<101, 103>(&input),
        start.elapsed()
    );
    start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2::<101, 103>(&input),
        start.elapsed()
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_1_INPUT: &str = "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3";
    const TEST_1_ANSWER: usize = 12;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1::<11, 7>(&parse(TEST_1_INPUT)), TEST_1_ANSWER);
    }
}
