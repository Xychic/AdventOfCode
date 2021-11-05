use std::fs;

fn main() {
    let input: String = fs::read_to_string("../../../input.txt").expect("error reading file");
    let lines: Vec<Vec<usize>> = 
        input
        .trim()
        .split("\n")
        .map(|line| {
            line
            .trim()
            .split("  ")
            .filter(|x| x != &"")
            .map(|val| {
                val
                .trim()
                .parse::<usize>()
                .expect("Nan")
            })
            .collect::<Vec<usize>>()
        })
        .collect();

    println!("Part 1: {}", part_1(lines.clone()));
    println!("Part 2: {}", part_2(lines.clone()));
}

fn part_1(lines: Vec<Vec<usize>>) -> usize {
    let mut ans = 0;

    for line in lines {
        let m = match line.iter().max() {
            Some(num) => num,
            None      => break
        };
        if line.iter().sum::<usize>() > 2 * m {
            ans += 1;
        }
    }
    ans
}

fn part_2(lines: Vec<Vec<usize>>) -> usize {
    let mut ans = 0;

    for i in (0..lines.len()).step_by(3) {
        for j in 0..3 {
            let vals = [lines[i][j], lines[i+1][j], lines[i+2][j]];
            let m = match vals.iter().max() {
                Some(num) => num,
                None      => break
            };
            if vals.iter().sum::<usize>() > 2 * m {
                ans += 1;
            }
        }
    }
    ans
}
