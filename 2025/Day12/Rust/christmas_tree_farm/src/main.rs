use christmas_tree_farm::{parse, part_1};
use std::{fs, time::Instant};

fn main() {
    let mut start = Instant::now();
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    println!("Parsed input in {:?}", start.elapsed());

    start = Instant::now();
    println!("Part 1: {:?}, took {:?}", part_1(&input), start.elapsed());
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_1_INPUT: &str = "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2";
    const TEST_1_ANSWER: usize = 2;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(TEST_1_INPUT)), TEST_1_ANSWER);
    }
}
