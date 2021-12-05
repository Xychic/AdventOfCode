use std::{collections::HashMap, fs, time::Instant};

type Input = Vec<((i16, i16), (i16, i16))>;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    let start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1(&input),
        Instant::now() - start
    );
    let start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2(&input),
        Instant::now() - start
    );
}

fn parse(input: &str) -> Input {
    input
        .trim()
        .split('\n')
        .map(|line| {
            let mut parts = line.split(" -> ");
            let mut a = parts.next().unwrap().split(',').map(|x| x.parse().unwrap());
            let mut b = parts.next().unwrap().split(',').map(|x| x.parse().unwrap());

            (
                (a.next().unwrap(), a.next().unwrap()),
                (b.next().unwrap(), b.next().unwrap()),
            )
        })
        .collect()
}

fn part_1(lines: &Input) -> usize {
    let mut overlaps = HashMap::new();

    for ((x1, y1), (x2, y2)) in lines {
        if x1 != x2 && y1 != y2 {
            continue;
        }
        for x in *x1.min(x2)..=*x1.max(x2) {
            for y in *y1.min(y2)..=*y1.max(y2) {
                *overlaps.entry((x, y)).or_insert(0) += 1;
            }
        }
    }
    overlaps.values().filter(|&&p| p >= 2).count()
}

fn part_2(lines: &Input) -> usize {
    let mut overlaps = HashMap::new();
    for (a, b) in lines {
        let (mut x1, mut y1) = *a;
        let (x2, y2) = *b;

        if x1 == x2 || y1 == y2 {
            for x in x1.min(x2)..=x1.max(x2) {
                for y in y1.min(y2)..=y1.max(y2) {
                    *overlaps.entry((x, y)).or_insert(0) += 1;
                }
            }
        } else {
            let dx = if x2 > x1 { 1 } else { -1 };
            let dy = if y2 > y1 { 1 } else { -1 };
            while x1 != x2 + dx {
                *overlaps.entry((x1, y1)).or_insert(0) += 1;
                x1 += dx;
                y1 += dy
            }
        }
    }

    overlaps.values().filter(|&&p| p >= 2).count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        let test_input = "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2";
        let test_answer = 5;
        assert_eq!(part_1(&parse(&test_input)), test_answer);
    }

    #[test]
    fn test_part_2() {
        let test_input = "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2";
        let test_answer = 12;
        assert_eq!(part_2(&parse(&test_input)), test_answer);
    }
}
