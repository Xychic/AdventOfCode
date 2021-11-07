use std::{cmp, fs};

#[derive(Debug, Clone, Copy)]
struct Node {
    pos: (usize, usize),
    size: usize,
    used: usize,
    available: usize,
}

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse_input(input.trim());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn part_1(input: &[Node]) -> usize {
    let mut total = 0;

    for (i, a) in input.iter().enumerate() {
        for b in input.iter().skip(i+1) {
            if a.used != 0 && a.used <= b.available {
                total += 1;
            }
            if b.used != 0 && b.used <= a.available {
                total += 1;

            }
        }
    }

    total
}

fn part_2(input: &[Node]) -> usize {
    let mut total_cost = 0;
    let x_size = input.iter().map(|n| n.pos.0).max().unwrap() + 1;
    let y_size = input.iter().map(|n| n.pos.1).max().unwrap() + 1;
    let mut grid = vec![vec![false; x_size]; y_size];
    let mut space = (0, 0);
    for n in input {
        let (x, y) = n.pos;
        if n.used == 0 {
            space = n.pos;
        }
        grid[y][x] = (n.used * 100 / n.size) >= 90;
    }
    let target = (x_size-1, 0);

    let mut wall_edge = (0, 0);
    for (index, row) in grid.iter().enumerate() {
        if row.contains(&true) {
            let closest = row.iter().enumerate().filter(|(_, &val)| !val).map(|(a,_)| a).max().unwrap();
            wall_edge = (closest, index);
            break;
        }
    }
    total_cost += get_taxicab_dist(wall_edge, space);
    total_cost += get_taxicab_dist(wall_edge, target);
    total_cost += 5 * (x_size-2);
    total_cost
}

fn parse_input(input: &str) -> Vec<Node> {
    input
    .split('\n')
    .collect::<Vec<_>>()[2..]
    .iter()
    .map(|line| {
        let mut line = line.to_string();
        while line.contains("  ") {
            line = line.replace("  ", " ");
        }
        line = line.replace("/dev/grid/node-", "");
        let data: Vec<_> = line.split(' ').collect();

        let mut pos = data[0].split('-').map(|x| { let mut c = x.chars(); c.next(); c.as_str() });
        let x: usize = pos.next().unwrap().parse().unwrap();
        let y: usize = pos.next().unwrap().parse().unwrap();

        let mut size = data[1].chars();
        size.next_back();
        let size: usize = size.as_str().parse().unwrap();

        let mut used = data[2].chars();
        used.next_back();
        let used: usize = used.as_str().parse().unwrap();

        Node{pos: (x, y), size, used, available: size-used}

    })
    .collect()
}

fn _print_grid(grid: &[Vec<bool>], space: (usize, usize), target: (usize, usize), end: (usize, usize)) {
    for (y, row) in grid.iter().enumerate() {
        for (x, wall) in row.iter().enumerate() {
            if (x, y) == space {
                print!("O");
            } else if (x, y) == target {
                print!("G");
            } else if (x, y) == end {
                print!("E");
            } else {
                print!("{}", if *wall {'#'} else {'.'});
            }
        }
        println!();
    }
}

fn get_taxicab_dist(a: (usize, usize), b: (usize, usize)) -> usize {
    cmp::max(a.0, b.0) - cmp::min(a.0, b.0) + cmp::max(a.1, b.1) - cmp::min(a.1, b.1)
}
