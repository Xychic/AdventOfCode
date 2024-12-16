use std::collections::HashMap;

type Input<'a> = Vec<Robot>;
type Point = (isize, isize);

#[derive(Debug, Clone, Copy)]
pub struct Robot {
    pos: Point,
    vel: Point,
}

impl Robot {
    fn step<const W: isize, const H: isize>(&mut self) {
        self.pos.0 = (self.pos.0 + self.vel.0 + W) % W;
        self.pos.1 = (self.pos.1 + self.vel.1 + H) % H;
    }

    fn get_quadrant<const W: isize, const H: isize>(&self) -> Option<(u8, u8)> {
        if self.pos.0 == W / 2 || self.pos.1 == H / 2 {
            return None;
        }
        Some((u8::from(self.pos.0 >= W / 2), u8::from(self.pos.1 >= H / 2)))
    }
}

/// Parser for 2024 Day 14 (`restroom_redoubt`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let (p, v) = l.split_once(' ').unwrap();
            let (p_x, p_y) = p.split_once(',').unwrap();
            let (v_x, v_y) = v.split_once(',').unwrap();

            Robot {
                pos: (p_x[2..].parse().unwrap(), p_y.parse().unwrap()),
                vel: (v_x[2..].parse().unwrap(), v_y.parse().unwrap()),
            }
        })
        .collect()
}

/// Solver for part 1 of 2024 Day 14 (`restroom_redoubt`)
///
/// # Panics
#[must_use]
pub fn part_1<const W: isize, const H: isize>(input: &Input) -> usize {
    let mut robots = input.to_owned();
    for _ in 0..100 {
        for r in &mut robots {
            r.step::<W, H>();
        }
    }

    let mut quadrants = HashMap::new();
    for r in robots {
        if let Some(quadrant) = r.get_quadrant::<W, H>() {
            *quadrants.entry(quadrant).or_insert(0) += 1;
        }
    }
    quadrants.values().product()
}

/// Solver for part 2 of 2024 Day 14 (`restroom_redoubt`)
///
/// # Panics
#[must_use]
pub fn part_2<const W: isize, const H: isize>(input: &Input) -> usize {
    let mut robots = input.to_owned();
    let mut quadrants = HashMap::new();
    let mut average = 0;
    let mut robot_pos_map = HashMap::new();
    for i in 1.. {
        for r in &mut robots {
            r.step::<W, H>();
            if let Some(quadrant) = r.get_quadrant::<W, H>() {
                *quadrants.entry(quadrant).or_insert(0) += 1;
            }
            *robot_pos_map.entry(r.pos).or_insert(0) += 1;
        }
        let score: usize = quadrants.values().product();
        average = (average * (i - 1) / i) + score / i;
        if average / score > 4 {
            for y in 0..H {
                for x in 0..W {
                    if let Some(count) = robot_pos_map.get(&(x, y)) {
                        print!("{count}");
                    } else {
                        print!(".");
                    }
                }
                println!();
            }
            return i;
        }
        quadrants.clear();
        robot_pos_map.clear();
    }
    unreachable!()
}
