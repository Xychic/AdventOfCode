use std::collections::HashSet;

type Input<'a> = Vec<(&'a str, usize)>;

pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let (dir, amount) = l.split_once(' ').unwrap();
            (dir, amount.parse().unwrap())
        })
        .collect()
}

pub fn part_1(input: &Input) -> usize {
    solve(input, 2)
}

pub fn part_2(input: &Input) -> usize {
    solve(input, 10)
}

fn solve(input: &Input, knot_count: usize) -> usize {
    let mut knots = vec![(0isize, 0isize); knot_count];
    let mut seen = HashSet::with_capacity(input.iter().map(|(_, x)| x).sum());
    seen.insert(knots[knot_count - 1]);

    for &(dir, amount) in input {
        for _ in 0..amount {
            let (px, py) = knots[0];
            knots[0] = match dir {
                "U" => (px, py + 1),
                "D" => (px, py - 1),
                "L" => (px - 1, py),
                "R" => (px + 1, py),
                _ => unreachable!(),
            };
            for i in 0..knot_count - 1 {
                let (x1, y1) = knots[i];
                let (x2, y2) = knots[i + 1];

                if x1.abs_diff(x2) > 1 || y1.abs_diff(y2) > 1 {
                    knots[i + 1] = (x2 + (x1 - x2).signum(), y2 + (y1 - y2).signum())
                }
            }
            seen.insert(knots[knot_count - 1]);
        }
    }

    seen.len()
}
