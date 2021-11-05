use std::fs;

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = input.trim().chars().map(|x| x == '1').collect();

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn part_1(input: &Vec<bool>) -> String {
    to_string(&checksum(&dragon(input, 272)))
}

fn part_2(input: &Vec<bool>) -> String {
    to_string(&checksum(&dragon(input, 35651584)))
}

fn dragon(input: &Vec<bool>, disk_size: usize) -> Vec<bool> {
    let mut result = dragon_step(input);
    let mut size = result.len();
    while size < disk_size {
        result = dragon_step(&result);
        size = size * 2 + 1;
    }
    result[0..disk_size].to_vec()
}

fn dragon_step(input: &Vec<bool>) -> Vec<bool> {    
    let size = input.len() * 2 + 1;
    let mut result = vec![false; size];
    for (index, c) in input.iter().enumerate() {
        result[index] = *c;
        result[size-index-1] = ! *c;
    }
    result
}

fn checksum(input: &Vec<bool>) -> Vec<bool> {
    let size = input.len() / 2;
    let mut result = Vec::with_capacity(size);
    let mut input_iter = input.iter();
    for _ in 0..size {
        let a = input_iter.next().unwrap();
        let b = input_iter.next().unwrap();
        result.push(!(a^b));
    }
    if size % 2 == 0 {
        result = checksum(&result);
    }

    result
}

fn to_string(input: &Vec<bool>) -> String {
    input.iter().map(|x| if *x {'1'} else {'0'}).collect()
}