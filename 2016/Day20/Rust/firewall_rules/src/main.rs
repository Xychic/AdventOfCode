use std::{cmp, fs};

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let mut input: Vec<(usize, usize)> = input
        .trim()
        .split('\n')
        .map(|x| {
                let d =
                x
                .split('-')
                .map(|y| y.parse::<usize>().unwrap())
                .collect::<Vec<_>>();
                (d[0], d[1])
        })
        .collect();
    input.sort_by(|(a,_), (b,_)| a.partial_cmp(b).unwrap());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn part_1(input: &[(usize, usize)]) -> usize {
    let mut current_ip = 0;
    let mut current_range = 0;

    while current_ip <= (u32::MAX) as usize {
        let (a, b) = input[current_range];
        if current_ip >= a {
            current_range += 1;
            current_ip = cmp::max(current_ip, b+1);
        } else {
            return current_ip;
        }
    }
    0
}

fn part_2(input: &[(usize, usize)]) -> usize {
    let mut current_ip = 0;
    let mut current_range = 0;
    let mut unblocked_ips = Vec::new();

    while current_ip <= (u32::MAX) as usize {
        let (a, b) = input[current_range];
        if current_ip >= a {
            current_range += 1;
            current_ip = cmp::max(current_ip, b+1);
        } else {
            unblocked_ips.push(current_ip);
            current_ip += 1;
        }
    }
    unblocked_ips.len()
}
