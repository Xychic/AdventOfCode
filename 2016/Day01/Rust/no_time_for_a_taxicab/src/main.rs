use std::fs;
use std::collections::HashSet;

fn main() {
    let input: String = fs::read_to_string("../../../input.txt").expect("error reading file");
    let moves: Vec<&str> = input.trim().split(", ").collect();

    println!("Part 1: {}", part_1(moves.clone()));
    println!("Part 2: {}", part_2(moves.clone()));
}

fn part_1(moves: Vec<&str>) -> isize {
    let mut hx: isize = 0;
    let mut hy: isize = 1;
    let mut x: isize = 0;
    let mut y: isize = 0;

    for m in moves {
        let dir = m.chars().nth(0).unwrap();
        let amt: isize = match m[1..].to_string().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("NaN: {}", &m[1..]);
                continue;
            }
        };

        if dir == 'R' {
            let nhx = hy;
            hy = -hx;
            hx = nhx;
        } else {
            let nhx = -hy;
            hy = hx;
            hx = nhx;
        }
        x += hx * amt;
        y += hy * amt;
    }
    x.abs() + y.abs()
}

fn part_2(moves: Vec<&str>) -> isize {
    let mut visited: HashSet<(isize, isize)> = HashSet::new();
    let mut hx: isize = 0;
    let mut hy: isize = 1;
    let mut x: isize = 0;
    let mut y: isize = 0;

    for m in moves {
        let dir = m.chars().nth(0).unwrap();
        let amt: isize = match m[1..].to_string().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("NaN: {}", &m[1..]);
                continue;
            }
        };

        if dir == 'R' {
            let nhx = hy;
            hy = -hx;
            hx = nhx;
        } else {
            let nhx = -hy;
            hy = hx;
            hx = nhx;
        }
        for _ in 0..amt {
            x += hx;
            y += hy;

            if visited.contains(&(x,y)) {
                return x.abs() + y.abs();
            }
            visited.insert((x, y));
        }

    }
    0
}
