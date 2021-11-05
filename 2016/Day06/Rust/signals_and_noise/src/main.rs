use std::fs;

fn main() {
    let input: String = fs::read_to_string("../../../input.txt").expect("error reading file");
    let lines: Vec<&str> = input.trim().split("\n").collect();

    println!("Part 1: {}", part_1(lines.clone()));
    println!("Part 2: {}", part_2(lines.clone()));
}

fn part_1(lines: Vec<&str>) -> String {
    let rows: usize = lines.len();
    let cols: usize = lines[0].len();

    let mut rotate: Vec<Vec<char>> = vec![vec!['.'; rows]; cols];

    for i in 0..rows {
        for j in 0..cols {
            rotate[j][i] = lines[i].chars().nth(j).unwrap();
        }
    }
    
    rotate
    .iter()
    .map(|col| {
        let mut sorted = col.clone();
        sorted.dedup();
        sorted.sort_by(|a, b| {col.iter().filter(|&c| *c==*b).count().cmp(&col.iter().filter(|&c| *c==*a).count())});
        sorted[0]
    })
    .collect::<String>()
}

fn part_2(lines: Vec<&str>) -> String {
    let rows: usize = lines.len();
    let cols: usize = lines[0].len();

    let mut rotate: Vec<Vec<char>> = vec![vec!['.'; rows]; cols];

    for i in 0..rows {
        for j in 0..cols {
            rotate[j][i] = lines[i].chars().nth(j).unwrap();
        }
    }
    
    rotate
    .iter()
    .map(|col| {
        let mut sorted = col.clone();
        sorted.dedup();
        sorted.sort_by(|a, b| {col.iter().filter(|&c| *c==*a).count().cmp(&col.iter().filter(|&c| *c==*b).count())});
        sorted[0]
    })
    .collect::<String>()
}
