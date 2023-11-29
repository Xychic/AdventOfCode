use sea_cucumber::{parse, part_1};
use std::{fs, time::Instant};

fn main() {
    let mut start = Instant::now();
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    println!("Parsed input in {:?}", start.elapsed());

    start = Instant::now();
    println!("Part 1: {}, took {:?}", part_1(&input), start.elapsed());
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_1_INPUT: &str = "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>";
    const TEST_1_ANSWER: usize = 58;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(TEST_1_INPUT)), TEST_1_ANSWER);
    }
}
