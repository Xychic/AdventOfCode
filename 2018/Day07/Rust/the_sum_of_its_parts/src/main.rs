use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap},
    fs,
};

type Input<'a> = Vec<(&'a str, &'a str)>;

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(input.trim());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn parse(input: &str) -> Input {
    input
        .split('\n')
        .map(|a| {
            let a: Vec<_> = a.split(' ').collect();
            (a[1], a[7])
        })
        .collect()
}

fn part_1(input: &Input) -> String {
    let mut blocking = HashMap::new();
    let mut blocked_by = HashMap::new();
    let mut to_process = BinaryHeap::new();
    for &(a, b) in input {
        blocking.entry(a).or_insert_with(Vec::new).push(b);
        *blocked_by.entry(b).or_insert(0) += 1;
    }

    for &k in blocking.keys() {
        if blocked_by.get(k).unwrap_or(&0) == &0 {
            to_process.push(Reverse(k));
        }
    }

    let mut ans = String::new();
    while !to_process.is_empty() {
        let current = to_process.pop().unwrap();
        ans += current.0;
        for &b in blocking.get(&current.0).unwrap_or(&Vec::new()) {
            *blocked_by.entry(b).or_insert(0) -= 1;
            if blocked_by.get(b).unwrap() == &0 {
                to_process.push(Reverse(b));
            }
        }
    }
    ans
}

fn part_2(input: &Input) -> usize {
    let mut blocking = HashMap::new();
    let mut blocked_by = HashMap::new();
    let mut to_process = BinaryHeap::new();
    let mut workers = HashMap::with_capacity(5);
    let mut alpha_pos = HashMap::with_capacity(26);

    for (i, c) in (b'A'..=b'Z').into_iter().enumerate() {
        alpha_pos.insert(String::from_utf8(vec![c]).unwrap(), i + 61);
    }

    for &(a, b) in input {
        blocking.entry(a).or_insert_with(Vec::new).push(b);
        *blocked_by.entry(b).or_insert(0) += 1;
    }

    for &k in blocking.keys() {
        if blocked_by.get(k).unwrap_or(&0) == &0 {
            to_process.push(Reverse(k));
        }
    }

    let mut time_taken = 0;
    let mut busy_workers = 0;

    while !to_process.is_empty() || busy_workers > 0 {
        if busy_workers < 5 && !to_process.is_empty() {
            let current = to_process.pop().unwrap().0;
            let work = alpha_pos.get(current).unwrap();
            workers.insert(current, *work);
            busy_workers += 1;
            continue;
        }

        for (&k, v) in workers.iter_mut() {
            if v == &mut 0 {
                continue;
            }
            *v -= 1;
            if v == &mut 0 {
                busy_workers -= 1;
                for &b in blocking.get(k).unwrap_or(&Vec::new()) {
                    *blocked_by.entry(b).or_insert(1) -= 1;
                    if blocked_by.get(b).unwrap() == &0 {
                        to_process.push(Reverse(b));
                    }
                }
            }
        }

        time_taken += 1;
    }
    time_taken
}
