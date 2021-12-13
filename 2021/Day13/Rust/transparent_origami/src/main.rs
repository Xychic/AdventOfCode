use std::{collections::HashSet, fs, time::Instant};

type Input<'a> = (HashSet<(usize, usize)>, Vec<(&'a str, usize)>);

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
    let mut parts = input.trim().split("\n\n");
    (
        parts
            .next()
            .unwrap()
            .lines()
            .map(|l| {
                let mut l = l.split(',');
                (
                    l.next().unwrap().parse().unwrap(),
                    l.next().unwrap().parse().unwrap(),
                )
            })
            .collect(),
        parts
            .next()
            .unwrap()
            .lines()
            .map(|l| {
                let mut l = l.split('=');

                (
                    l.next().unwrap().get(11..).unwrap(),
                    l.next().unwrap().parse().unwrap(),
                )
            })
            .collect(),
    )
}

fn part_1((dots, rules): &Input) -> usize {
    let dots = dots.to_owned();

    let (axis, val) = rules[0];
    let mut new_dots = HashSet::with_capacity(dots.len());
    match axis {
        "x" => {
            for (x, y) in dots {
                new_dots.insert((if x > val { 2 * val - x } else { x }, y));
            }
        }
        "y" => {
            for (x, y) in dots {
                new_dots.insert((x, if y > val { 2 * val - y } else { y }));
            }
        }
        _ => unreachable!(),
    }

    new_dots.len()
}

fn part_2((dots, rules): &Input) -> String {
    let mut dots = dots.to_owned();

    for &(axis, val) in rules {
        let mut new_dots = HashSet::with_capacity(dots.len());
        match axis {
            "x" => {
                for (x, y) in dots {
                    new_dots.insert((if x > val { 2 * val - x } else { x }, y));
                }
            }
            "y" => {
                for (x, y) in dots {
                    new_dots.insert((x, if y > val { 2 * val - y } else { y }));
                }
            }
            _ => unreachable!(),
        }
        dots = new_dots;
    }
    let max_y = dots.iter().max_by_key(|(_, y)| y).unwrap().1;
    let max_x = dots.iter().max_by_key(|(x, _)| x).unwrap().0;
    let mut ans = String::with_capacity((max_x + 1) * max_y * 2);
    for y in 0..=max_y {
        ans += "\n";
        for x in 0..=max_x {
            ans += if dots.contains(&(x, y)) { "#" } else { " " }
        }
    }

    ans
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "
6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 17);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(
            part_2(&parse(&TEST_INPUT_2)),
            String::from(
                "
#####
#   #
#   #
#   #
#####"
            )
        );
    }
}
