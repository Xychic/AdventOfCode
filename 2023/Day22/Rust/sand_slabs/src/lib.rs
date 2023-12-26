use std::collections::HashSet;

type Point = (usize, usize, usize);
type Input<'a> = Vec<Vec<Point>>;

/// Parser for 2023 Day 22 (`sand_slabs`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut parsed: Input = input
        .trim()
        .lines()
        .map(|l| {
            let (start, end) = l.split_once('~').unwrap();
            let [sx, sy, sz] = start
                .split(',')
                .map(|c| c.parse().unwrap())
                .collect::<Vec<_>>()[..3]
            else {
                unreachable!()
            };
            let [ex, ey, ez] = end
                .split(',')
                .map(|c| c.parse().unwrap())
                .collect::<Vec<_>>()[..3]
            else {
                unreachable!()
            };
            match (sx == ex, sy == ey, sz == ez) {
                (_, true, true) => (sx..=ex).map(|x| (x, sy, sz)).collect(),
                (true, _, true) => (sy..=ey).map(|y| (sx, y, sz)).collect(),
                (true, true, _) => (sz..=ez).map(|z| (sx, sy, z)).collect(),
                _ => unreachable!(),
            }
        })
        .collect();

    let mut all_blocks: HashSet<_> = parsed.iter().flatten().copied().collect();

    loop {
        let mut changes = false;
        for block in &mut parsed {
            let mut can_fall = true;
            for &(x, y, z) in block.iter() {
                let supporting = (x, y, z - 1);
                if z == 0 || (!block.contains(&supporting) && all_blocks.contains(&supporting)) {
                    can_fall = false;
                    break;
                }
            }
            if can_fall {
                changes = true;
                for &(x, y, z) in block.iter() {
                    all_blocks.remove(&(x, y, z));
                    all_blocks.insert((x, y, z - 1));
                }
                *block = block
                    .clone()
                    .iter()
                    .map(|&(x, y, z)| (x, y, z - 1))
                    .collect();
            }
        }
        if !changes {
            break;
        }
    }

    parsed
}

/// Solver for part 1 of 2023 Day 22 (`sand_slabs`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut ans = 0;
    for (i, block) in input.iter().enumerate() {
        let blocks_left: HashSet<_> = input
            .iter()
            .flatten()
            .filter(|x| !block.contains(x))
            .copied()
            .collect();
        let mut can_remove = true;
        for (j, test_block) in input.iter().enumerate() {
            if i == j {
                continue;
            }
            let mut can_fall = true;
            for &(x, y, z) in test_block {
                let supporting = (x, y, z - 1);
                if z == 0
                    || (!test_block.contains(&supporting) && blocks_left.contains(&supporting))
                {
                    can_fall = false;
                    break;
                }
            }
            if can_fall {
                can_remove = false;
                break;
            }
        }
        if can_remove {
            ans += 1;
        }
    }

    ans
}

/// Solver for part 2 of 2023 Day 22 (`sand_slabs`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut ans = 0;
    for (i, block) in input.iter().enumerate() {
        let mut blocks_left: HashSet<_> = input
            .iter()
            .flatten()
            .filter(|x| !block.contains(x))
            .copied()
            .collect();
        let mut blocks = input.clone();
        let mut will_fall = HashSet::new();
        loop {
            let mut changes = false;
            for (j, block) in blocks.iter_mut().enumerate() {
                if j == i {
                    continue;
                }
                let mut can_fall = true;
                for &(x, y, z) in block.iter() {
                    let supporting = (x, y, z - 1);
                    if z == 0 || (!block.contains(&supporting) && blocks_left.contains(&supporting))
                    {
                        can_fall = false;
                        break;
                    }
                }
                if can_fall {
                    will_fall.insert(j);
                    changes = true;
                    for &(x, y, z) in block.iter() {
                        blocks_left.remove(&(x, y, z));
                        blocks_left.insert((x, y, z - 1));
                    }
                    *block = block
                        .clone()
                        .iter()
                        .map(|&(x, y, z)| (x, y, z - 1))
                        .collect();
                }
            }
            if !changes {
                break;
            }
        }
        ans += will_fall.len();
    }

    ans
}
