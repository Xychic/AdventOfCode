use std::fs;

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = input.trim();

    println!("Part 1: {}", part_1(input));
    println!("Part 2: {}", part_2(input));
}

fn part_1(input: &str) -> usize {
    let mut safe = 0;
    let mut current = input.to_string();
    safe += current.matches('.').count();

    for _ in 1..40 {
        current = get_next_row(&current);
        safe += current.matches('.').count();
    }
    safe
}

fn part_2(input: &str) -> usize {
    let mut safe = 0;
    let mut current = input.to_string();
    safe += current.matches('.').count();

    for _ in 1..400000 {
        current = get_next_row(&current);
        safe += current.matches('.').count();
    }
    safe
}

fn get_next_row(input: &str) -> String {
    let size = input.len();
    let mut result = Vec::with_capacity(size);
    
    for i in 0..size {
        if i == 0 {
            let s = &input[i..i+2];
            if s == "^^" || s == ".^" { result.push('^'); } 
            else { result.push('.'); }
        } else if i == size-1 {
            let s = &input[i-1..i+1];
            if s == "^^" || s == "^." { result.push('^'); } 
            else { result.push('.'); }
        } else {
            let s = &input[i-1..i+2];
            if s == "^^." || s == ".^^" || s == "^.." || s == "..^" { result.push('^'); } 
            else { result.push('.'); }
        }
    }

    result.iter().collect::<String>()
}
