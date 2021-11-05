use std::{collections::VecDeque, fs};

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = input.trim().parse().unwrap();

    println!("Part 1: {}", part_1(input));
    println!("Part 2: {}", part_2(input));
}

fn part_1(input: usize) -> usize {
    let mut queue = VecDeque::with_capacity(input);
    for i in 0..input { queue.push_back(i); }

    while queue.len() > 1 {
        let a = queue.pop_front().unwrap();
        queue.push_back(a);
        queue.pop_front();
    }
    queue.pop_front().unwrap() + 1
}

fn part_2(input: usize) -> usize {
    let mut left_queue = VecDeque::with_capacity(input / 2);
    let mut right_queue = VecDeque::with_capacity(input / 2);

    for i in 0..input {
        if i < (input / 2) + 1 { left_queue.push_back(i+1); } 
        else { right_queue.push_back(i+1); }
    }

    while (!left_queue.is_empty()) && (!right_queue.is_empty()) {
        if left_queue.len() > right_queue.len() {
            left_queue.pop_back();
        } else {
            right_queue.pop_back();
        }

        right_queue.push_front(left_queue.pop_front().unwrap());
        left_queue.push_back(right_queue.pop_back().unwrap());
    }

    if !left_queue.is_empty() {
        left_queue.pop_front().unwrap()
    } else {
        right_queue.pop_front().unwrap()
    }
}
