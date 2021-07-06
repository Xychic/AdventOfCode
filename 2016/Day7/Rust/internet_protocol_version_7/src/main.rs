use std::fs;
use std::collections::HashSet;

fn main() {
    let input: String = fs::read_to_string("../../../input.txt").expect("error reading file");
    let lines: Vec<Vec<&str>> = 
        input
        .trim()
        .split("\n")
        .map(|line| {
            line
            .split(&['[',']'][..])
            .collect::<Vec<&str>>()
        })
        .collect();

    println!("Part 1: {}", part_1(lines.clone()));
    println!("Part 2: {}", part_2(lines.clone()));
}

fn part_1(lines: Vec<Vec<&str>>) -> usize {
    let mut ans = 0;

    for line in lines {
        let mut valid = false;
        for (i, word) in line.iter().enumerate() {
            if is_abba_ish(word) {
                if i % 2 == 0 { valid = true; } 
                else {
                    valid = false;
                    break;
                }
            }
        }
        if valid { ans += 1; }
    }

    ans
}

fn part_2(lines: Vec<Vec<&str>>) -> usize {
    let mut ans = 0;

    for line in lines {
        let mut out = HashSet::new();
        let mut ins = HashSet::new();

        for (i, word) in line.iter().enumerate() {
            let valid = is_aba_ish(word);
            if valid.len() == 0 { continue; }

            if i % 2 == 0 { out.extend(valid.iter().cloned()); } 
            else { ins.extend(valid.iter().cloned()); }
        }

        for w in out {
            let mut search = w[1..].to_string();
            search.push(w.chars().nth(1).unwrap());
            if ins.contains(&search) {
                ans +=1;
                break;
            }
        }
    }

    ans
}

fn is_abba_ish(word: &str) -> bool {
    for c in 0..word.len()-3 {
        let w = &word[c..c+4];

        if w == w.chars().rev().collect::<String>() 
            && w.chars().nth(0) != w.chars().nth(1) {
                return true;
        }
    }
    false
}

fn is_aba_ish(word: &str) -> Vec<String> {
    let mut valid = HashSet::new();

    for c in 0..word.len()-2 {
        let w = &word[c..c+3];

        if w == w.chars().rev().collect::<String>() 
            && w.chars().nth(0) != w.chars().nth(1) {
                valid.insert(w.to_string());
        }
    }
    
    valid.into_iter().collect::<Vec<String>>()
}
