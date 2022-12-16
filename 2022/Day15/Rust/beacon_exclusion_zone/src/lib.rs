use std::collections::HashSet;

use itertools::Itertools;

type Point = (isize, isize);
type Input = (HashSet<(Point, usize)>, HashSet<Point>);

pub fn parse(input: &str) -> Input {
    let mut sensors = HashSet::new();
    let mut beacons = HashSet::new();
    for line in input.trim().lines() {
        let (s, b) = line.split_once(": closest beacon is at x=").unwrap();
        let (sx, sy) = s[12..].split_once(", y=").unwrap();
        let (bx, by) = b.split_once(", y=").unwrap();

        let sx = sx.parse().unwrap();
        let sy = sy.parse().unwrap();
        let bx = bx.parse().unwrap();
        let by = by.parse().unwrap();
        sensors.insert(((sx, sy), dist((sx, sy), (bx, by))));
        beacons.insert((bx, by));
    }

    (sensors, beacons)
}

fn dist(a: Point, b: Point) -> usize {
    a.0.abs_diff(b.0) + a.1.abs_diff(b.1)
}

pub fn part_1((sensors, beacons): &Input, y: isize) -> usize {
    let min_x = sensors
        .iter()
        .map(|&((x, _), d)| x - d as isize)
        .min()
        .unwrap();
    let max_x = sensors
        .iter()
        .map(|&((x, _), d)| x + d as isize)
        .max()
        .unwrap();
    let mut x = min_x;
    let mut total = 0;

    'outer: while x <= max_x {
        if beacons.contains(&(x, y)) {
            x += 1;
            continue;
        }
        for &(s @ (sx, sy), d) in sensors {
            if dist(s, (x, y)) <= d {
                total += 1;
                x += 1;
                while dist(s, (x, y)) < d {
                    x += 1;
                    total += 1;
                }
                continue 'outer;
            }
        }
        x += 1;
    }

    total
}

pub fn part_2((sensors, _): &Input, max_val: isize) -> usize {
    sensors
        .iter()
        .cartesian_product(sensors.iter().skip(1))
        .map(|(&((s1x, s1y), s1d), &((s2x, s2y), s2d))| {
            [s1x + s1y + s1d as isize + 1, s1x + s1y - (s1d as isize + 1)]
                .iter()
                .cartesian_product([s2x - s2y + s2d as isize + 1, s2x - s2y - (s2d as isize + 1)])
                .map(|(a, b)| {
                    let x = (*a + b) / 2;
                    let y = *a - x;
                    (x, y)
                })
                .find(|&(x, y)| {
                    0 <= x
                        && x <= max_val
                        && 0 <= y
                        && y <= max_val
                        && sensors.iter().all(|&(s, d)| dist(s, (x, y)) > d)
                })
        })
        .find(|x| x.is_some())
        .map(|o| {
            let (x, y) = o.unwrap();
            (4000000 * x + y) as usize
        })
        .unwrap()
}
