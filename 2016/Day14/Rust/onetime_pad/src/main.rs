use std::{cmp, fs};
use std::collections::{HashMap, HashSet};

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = input.trim();

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn part_1(input: &str) -> usize {
    let mut three_hashes: HashMap<String, Vec<usize>> = HashMap::new();
    let mut keys = Vec::new();
    let mut max_key = 0;

    for index in 0.. {
        if keys.len() > 64 && (index - max_key) >= 1000 {
            break;
        }
        let val = create_digest(&format!("{}{}",input, index));

        match has_n_repeat(&val, 5) {
            Some(x) => {
                for i in three_hashes.get(&x).unwrap_or(&Vec::new()) {
                    if (index - i) <= 1000 {
                        keys.push(*i);
                        max_key = cmp::max(max_key, *i);
                    }
                }
                three_hashes.insert(x,Vec::new());
            }
            None => {}
        }

        match has_n_repeat(&val, 3) {
            Some(x) => {
                three_hashes.entry(x).or_insert(Vec::new()).push(index);
            }
            None => continue
        }
    }

    keys.sort();

    keys[63]
}

fn part_2(input: &str) -> usize {
    let mut three_hashes: HashMap<String, Vec<usize>> = HashMap::new();
    let mut keys = Vec::new();
    let mut max_key = 0;

    for index in 0.. {
        if keys.len() > 64 && (index - max_key) >= 1000 {
            break;
        }
        let mut val = create_digest(&format!("{}{}",input, index));
        for _ in 0..2016 {
            val = create_digest(&val);
        }

        match has_n_repeat(&val, 5) {
            Some(x) => {
                for i in three_hashes.get(&x).unwrap_or(&Vec::new()) {
                    if (index - i) <= 1000 {
                        keys.push(*i);
                        max_key = cmp::max(max_key, *i);
                        println!("Found hash: {} {}", index, i);
                    }
                }
                three_hashes.insert(x,Vec::new());
            }
            None => {}
        }

        match has_n_repeat(&val, 3) {
            Some(x) => {
                three_hashes.entry(x).or_insert(Vec::new()).push(index);
            }
            None => continue
        }
    }

    keys.sort();

    keys[63]
}

fn create_digest(input: &str) -> String {
    let result = md5::compute(input.as_bytes());
    format!("{:x}", result)
}

fn has_n_repeat(digest: &str, n: usize) -> Option<String> {
    let size = digest.chars().count() - (n-1);

    for i in 0..size {
        let mut set = HashSet::new();
        let mut c = digest[i..i+n].chars();
        for _ in 0..n {
            set.insert(c.next().unwrap());
        }
        if set.len() == 1 {
            return Some(digest[i..i+1].to_string());
        }
    }
    None
}
