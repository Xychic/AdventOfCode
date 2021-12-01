use std::fs;

fn main() {
    let input: String = fs::read_to_string("../../../input.txt").expect("error reading file");
    let lines: Vec<&str> = input.trim().split("\n").collect();

    println!("Part 1: {}", part_1(lines.clone()));
    println!("Part 2: {}", part_2(lines.clone()));
}

fn part_1(lines: Vec<&str>) -> usize {
    0
}

fn part_2(lines: Vec<&str>) -> usize {
    0
}
