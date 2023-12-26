use pulse_propagation::{parse, part_1, part_2};
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
    const TEST_1_INPUT: &str = "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output";
    const TEST_1_ANSWER: usize = 11_687_500;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(TEST_1_INPUT)), TEST_1_ANSWER);
    }
}
