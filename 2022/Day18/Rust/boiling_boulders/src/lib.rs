use std::collections::{HashSet, VecDeque};

use itertools::Itertools;

type Point = (usize, usize, usize);
type Input = Vec<Point>;

pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let mut part_iter = l.split(',');
            (
                part_iter.next().unwrap().parse().unwrap(),
                part_iter.next().unwrap().parse().unwrap(),
                part_iter.next().unwrap().parse().unwrap(),
            )
        })
        .collect()
}

pub fn part_1(input: &Input) -> usize {
    input.len() * 6
        - 2 * input
            .iter()
            .combinations(2)
            .filter(|c| {
                let &(x1, y1, z1) = c[0];
                let &(x2, y2, z2) = c[1];
                [x1 == x2, y1 == y2, z1 == z2]
                    .iter()
                    .filter(|&&x| x)
                    .count()
                    == 2
                    && [
                        x1.abs_diff(x2) == 1,
                        y1.abs_diff(y2) == 1,
                        z1.abs_diff(z2) == 1,
                    ]
                    .iter()
                    .filter(|&&x| x)
                    .count()
                        == 1
            })
            .count()
}

fn is_external(
    point: &Point,
    cubes: &HashSet<Point>,
    _bounds @ (max_x, min_x, max_y, min_y, max_z, min_z): (
        usize,
        usize,
        usize,
        usize,
        usize,
        usize,
    ),
    inside_set: &mut HashSet<Point>,
    outside_set: &mut HashSet<Point>,
) -> bool {
    if outside_set.contains(point) {
        return true;
    } else if inside_set.contains(point) {
        return false;
    }
    let mut seen = HashSet::new();
    let mut queue = VecDeque::new();

    queue.push_back(*point);
    while let Some((x, y, z)) = queue.pop_front() {
        if seen.contains(&(x, y, z)) || cubes.contains(&(x, y, z)) {
            continue;
        }
        seen.insert((x, y, z));
        if x > max_x || x < min_x || y > max_y || y < min_y || z > max_z || z < min_z {
            for p in seen {
                outside_set.insert(p);
            }
            return true;
        }
        queue.push_back((x.wrapping_sub(1), y, z));
        queue.push_back((x + 1, y, z));
        queue.push_back((x, y.wrapping_sub(1), z));
        queue.push_back((x, y + 1, z));
        queue.push_back((x, y, z.wrapping_sub(1)));
        queue.push_back((x, y, z + 1));
    }
    for p in seen {
        inside_set.insert(p);
    }
    false
}

pub fn part_2(input: &Input) -> usize {
    let mut max_x = 0;
    let mut min_x = 0;
    let mut max_y = 0;
    let mut min_y = 0;
    let mut max_z = 0;
    let mut min_z = 0;

    let point_set: HashSet<_> = input
        .iter()
        .map(|&(x, y, z)| {
            max_x = max_x.max(x);
            min_x = min_x.min(x);
            max_y = max_y.max(y);
            min_y = min_y.min(y);
            max_z = max_z.max(z);
            min_z = min_z.min(z);
            (x, y, z)
        })
        .collect();

    let mut outside_set = HashSet::with_capacity(input.len());
    let mut inside_set = HashSet::with_capacity(input.len());

    input
        .iter()
        .map(|&(x, y, z)| {
            [
                (x.wrapping_sub(1), y, z),
                (x + 1, y, z),
                (x, y.wrapping_sub(1), z),
                (x, y + 1, z),
                (x, y, z.wrapping_sub(1)),
                (x, y, z + 1),
            ]
            .iter()
            .filter(|&p| {
                is_external(
                    p,
                    &point_set,
                    (max_x, min_x, max_y, min_y, max_z, min_z),
                    &mut inside_set,
                    &mut outside_set,
                )
            })
            .count()
        })
        .sum()
}
