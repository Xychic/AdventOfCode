use cathoderay_tube::*;
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
        "Part 2: \n{}\ntook {:?}",
        part_2(&input),
        Instant::now() - start
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_1_ANSWER: isize = 13140;
    const TEST_2_ANSWER: &str = "██░░██░░██░░██░░██░░██░░██░░██░░██░░██░░
███░░░███░░░███░░░███░░░███░░░███░░░███░
████░░░░████░░░░████░░░░████░░░░████░░░░
█████░░░░░█████░░░░░█████░░░░░█████░░░░░
██████░░░░░░██████░░░░░░██████░░░░░░████
███████░░░░░░░███████░░░░░░░███████░░░░░";

    #[test]
    fn test_part_1() {
        let test_1_input = fs::read_to_string("../../example.txt").expect("error reading file");
        assert_eq!(part_1(&parse(&test_1_input)), TEST_1_ANSWER);
    }

    #[test]
    fn test_part_2() {
        let test_2_input = fs::read_to_string("../../example.txt").expect("error reading file");
        assert_eq!(part_2(&parse(&test_2_input)), TEST_2_ANSWER);
    }
}
