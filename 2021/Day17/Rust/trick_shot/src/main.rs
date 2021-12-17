use std::{cmp::Ordering, fs, time::Instant};

use regex::Regex;

#[derive(Debug)]
struct Probe {
    pos: (isize, isize),
    vel: (isize, isize),
}

impl Probe {
    fn new(pos: (isize, isize), vel: (isize, isize)) -> Probe {
        Probe { pos, vel }
    }

    fn step(&mut self) {
        self.pos.0 += self.vel.0;
        self.pos.1 += self.vel.1;
        self.vel.0 += match self.vel.0.cmp(&0) {
            Ordering::Less => 1,
            Ordering::Equal => 0,
            Ordering::Greater => -1,
        };
        self.vel.1 -= 1
    }

    fn check_hit(&self, ((x1, x2), (y1, y2)): &Input) -> bool {
        x1 <= &self.pos.0 && &self.pos.0 <= x2 && y1 <= &self.pos.1 && &self.pos.1 <= y2
    }
}

type Input = ((isize, isize), (isize, isize));

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
    let re =
        Regex::new(r"target area: x=(-?[0-9]+)..(-?[0-9]+), y=(-?[0-9]+)..(-?[0-9]+)").unwrap();
    let caps = re.captures(input.trim()).unwrap();
    (
        (caps[1].parse().unwrap(), caps[2].parse().unwrap()),
        (caps[3].parse().unwrap(), caps[4].parse().unwrap()),
    )
}

fn part_1(((x1, x2), (y1, y2)): &Input) -> isize {
    // If sqrt(x_max) < abs(y_min) we can ignore the x values
    if (*x2 as f64).sqrt() < y1.abs() as f64 {
        return (y1.abs() * (y1.abs() - 1)) / 2;
    }
    // If not, we have to simulate
    for y in (*y1 + 1)..0 {
        for x in 1..=*x2 {
            let mut p = Probe::new((0, 0), (x, y));
            while &p.pos.0 <= x2 && y1 <= &p.pos.1 {
                p.step();
                if p.check_hit(&((*x1, *x2), (*y1, *y2))) {
                    return (y.abs() * (y.abs() - 1)) / 2;
                }
            }
        }
    }
    unreachable!()
}

fn part_2(((x1, x2), (y1, y2)): &Input) -> usize {
    let mut ans = 0;
    for y in -y1.abs()..y1.abs() {
        for x in 1..=*x2 {
            let mut p = Probe::new((0, 0), (x, y));
            while &p.pos.0 <= x2 && y1 <= &p.pos.1 {
                p.step();
                if p.check_hit(&((*x1, *x2), (*y1, *y2))) {
                    ans += 1;
                    break;
                }
            }
        }
    }
    ans
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "target area: x=20..30, y=-10..-5";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 45);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 112);
    }
}
