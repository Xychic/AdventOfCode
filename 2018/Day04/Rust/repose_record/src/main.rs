use std::{collections::HashMap, fs};

#[derive(Debug)]
struct Entry {
    guard: usize,
    sleep: usize,
    wake: usize,
}

type Input = Vec<Entry>;

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(input.trim());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn parse(input: &str) -> Input {
    let mut lines: Vec<_> = input.split('\n').collect();
    lines.sort();
    let mut res = Vec::with_capacity(lines.len());
    let mut guard = 0;
    let mut sleep = 0;
    for line in lines {
        let time = line
            .chars()
            .into_iter()
            .skip(15)
            .take(2)
            .collect::<String>()
            .parse()
            .unwrap();
        if line.contains("begins shift") {
            guard = line.replace("#", "").split(' ').collect::<Vec<_>>()[3]
                .parse()
                .unwrap();
        } else if line.contains("falls asleep") {
            sleep = time;
        } else if line.contains("wakes up") {
            res.push(Entry {
                guard,
                sleep,
                wake: time,
            });
        }
    }
    res
}

fn part_1(input: &Input) -> usize {
    let mut sleep_log = HashMap::with_capacity(input.len());
    for entry in input {
        for time in entry.sleep..entry.wake {
            sleep_log
                .entry(entry.guard)
                .or_insert(Vec::new())
                .push(time);
        }
    }
    let ans = sleep_log.iter().max_by_key(|(_, a)| a.len()).unwrap();
    let mut count = HashMap::with_capacity(ans.1.len());
    for time in ans.1 {
        *count.entry(time).or_insert(0) += 1;
    }
    ans.0 * *count.iter().max_by_key(|(_, &a)| a).unwrap().0
}

fn part_2(input: &Input) -> usize {
    let mut sleep_log = HashMap::with_capacity(input.len());
    for entry in input {
        for time in entry.sleep..entry.wake {
            *sleep_log
                .entry(entry.guard)
                .or_insert(HashMap::new())
                .entry(time)
                .or_insert(0) += 1;
        }
    }
    let ans = sleep_log
        .iter()
        .max_by_key(|(_, a)| a.values().max().unwrap())
        .unwrap();
    ans.0 * *ans.1.iter().max_by_key(|(_, &a)| a).unwrap().0
}
