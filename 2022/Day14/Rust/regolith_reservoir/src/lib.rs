use std::collections::HashSet;

type Point = (usize, usize);
type Input = (usize, HashSet<Point>);

pub fn parse(input: &str) -> Input {
    let mut rock = HashSet::new();
    let mut base = 0;
    for line in input.trim().lines().collect::<HashSet<_>>().iter() {
        let parts: Vec<_> = line
            .split(" -> ")
            .map(|p| {
                let (x, y) = p.split_once(',').unwrap();
                (
                    x.parse::<usize>().unwrap(),
                    y.parse::<usize>().unwrap(),
                )
            })
            .collect();
        for (start, end) in parts.iter().zip(parts.iter().skip(1)) {
            let max_y = start.1.max(end.1);
            base = base.max(max_y);
            if start.0 == end.0 {
                for y in start.1.min(end.1)..=max_y {
                    rock.insert((start.0, y ));
                }
            } else {
                for x in start.0.min(end.0)..=start.0.max(end.0) {
                    rock.insert((x, start.1 ));
                }
            }
        }
    }

    (base, rock)
}

pub fn part_1((base, points): &Input) -> usize {
    let mut points = points.to_owned();
    let mut path = vec![(500, 0)];
    for sand_count in 0..50_000 {
        let (mut x, mut y) = path.pop().unwrap();
        loop {
            path.push((x, y));
            if y >= *base {
                return sand_count;
            }
            if !points.contains(&(x, y + 1 )) {
                y += 1;
            } else if !points.contains(&(x - 1, y + 1 )) {
                x -= 1;
                y += 1;
            } else if !points.contains(&(x + 1, y + 1 )) {
                x += 1;
                y += 1;
            } else {
                break;
            }
        }
        points.insert(path.pop().unwrap());
    }
    unreachable!()
}

pub fn part_2((base, points): &Input) -> usize {
    let mut points = points.to_owned();
    let mut path = vec![(500, 0)];
    for sand_count in 1.. {
        let (mut x, mut y) = path.pop().unwrap();
        loop {
            path.push((x, y));
            if y + 1 >= *base + 2 {
                break;
            }
            if !points.contains(&(x, y + 1 )) {
                y += 1;
            } else if !points.contains(&(x - 1, y + 1 )) {
                x -= 1;
                y += 1;
            } else if !points.contains(&(x + 1, y + 1 )) {
                x += 1;
                y += 1;
            } else {
                break;
            }
        }
        if x == 500 && y == 0 {
            return sand_count;
        }
        points.insert(path.pop().unwrap());
    }
    unreachable!()
}
