use std::fs;
use std::cmp::{max, min};

fn main() {
    let input: String = fs::read_to_string("../../../input.txt").expect("error reading file");
    let lines: Vec<&str> = input.trim().split("\n").collect();

    println!("Part 1: {}", part_1(lines.clone()));
    println!("Part 2: {}", part_2(lines.clone()));
}

fn part_1(lines: Vec<&str>) -> String {
    let keys = [
        ['7', '8', '9'],
        ['4', '5', '6'],
        ['1', '2', '3']
    ];

    let mut x: isize = 1;
    let mut y: isize = 1;

    let ans: Vec<char> = lines.iter().map(|line| {
        let instructions: Vec<char> = line.chars().collect();
        for ins in instructions {
            if ins == 'U' {y += 1;}
            else if ins == 'R' {x += 1;}
            else if ins == 'D' {y -= 1;}
            else if ins == 'L' {x -= 1;}
            x = max(min(x, 2), 0);
            y = max(min(y, 2), 0);
        }
        keys[y as usize][x as usize]
    }).collect();
    ans.iter().collect()
}

fn part_2(lines: Vec<&str>) -> String {
    let keys = [
        ['x','x','x','x','x','x','x'],
        ['x','x','x','D','x','x','x'],
        ['x','x','A','B','C','x','x'],
        ['x','5','6','7','8','9','x'],
        ['x','x','2','3','4','x','x'],
        ['x','x','x','1','x','x','x'],
        ['x','x','x','x','x','x','x']

    ];

    let mut x: isize = 1;
    let mut y: isize = 3;

    let ans: Vec<char> = lines.iter().map(|line| {
        let instructions: Vec<char> = line.chars().collect();
        for ins in instructions {
            if ins == 'U' {
                y += 1;
                if keys[y as usize][x as usize] == 'x' {y -= 1;}
            }
            else if ins == 'R' {
                x += 1;
                if keys[y as usize][x as usize] == 'x' {x -= 1;}
            }
            else if ins == 'D' {
                y -= 1;
                if keys[y as usize][x as usize] == 'x' {y += 1;}
            }
            else if ins == 'L' {
                x -= 1;
                if keys[y as usize][x as usize] == 'x' {x += 1;}
            }
        }
        keys[y as usize][x as usize]
    }).collect();
    ans.iter().collect()
}
