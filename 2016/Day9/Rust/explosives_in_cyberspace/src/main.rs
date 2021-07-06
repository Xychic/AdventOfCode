use std::fs;

fn main() {
    let input: String = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = input.trim().to_string();

    println!("Part 1: {}", part_1(input.clone()));
    println!("Part 2: {}", part_2(input.clone()));
}

fn part_1(input: String) -> usize {
    let mut ans = 0;
    let mut in_marker = false;
    let mut marker = String::new();
    
    let mut i = 0;
    while i < input.len() {
        let c = input.chars().nth(i).unwrap();

        if ! in_marker && c == '(' {in_marker = true;} 
        else if in_marker {
            if c == ')' { 
                let params: Vec<usize> = 
                    marker
                    .split("x")
                    .map(|x| {
                        x
                        .parse::<usize>()
                        .expect("NaN")
                    })
                    .collect();

                ans += params[1] * params[0];

                i += params[0];
                marker = String::new();
                in_marker = false; 
            }
            else { marker.push(c); }
        } else { ans += 1; }
        i += 1;
    }
    ans
}

fn part_2(input: String) -> usize {
    let mut ans = 0;
    let mut in_marker = false;
    let mut marker = String::new();

    let mut i = 0;
    while i < input.len() {
        let c = input.chars().nth(i).unwrap();

        if ! in_marker && c == '(' {in_marker = true;} 
        else if in_marker {
            if c == ')' { 
                let params: Vec<usize> = 
                    marker
                    .split("x")
                    .map(|x| {
                        x
                        .parse::<usize>()
                        .expect("NaN")
                    })
                    .collect();

                ans += params[1] * part_2(input[i+1..i+params[0]+1].to_string());

                i += params[0];
                marker = String::new();
                in_marker = false; 
            }
            else { marker.push(c); }
        } else { ans += 1; }
        i += 1;
    }
    ans
}
