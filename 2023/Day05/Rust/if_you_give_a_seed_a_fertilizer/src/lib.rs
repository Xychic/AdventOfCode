type Input<'a> = (Vec<usize>, [Vec<(usize, usize, usize)>; 7]);

#[must_use]
pub fn get(item: usize, map: &[(usize, usize, usize)]) -> usize {
    for &(dest, start, range) in map {
        if start <= item && item < start + range {
            return item + dest - start;
        }
    }
    item
}

#[must_use]
pub fn get_range(range: &[(usize, usize)], map: &[(usize, usize, usize)]) -> Vec<(usize, usize)> {
    let mut result = Vec::new();
    let mut q = range.to_vec();

    for &(dest, start, range) in map {
        let end = start + range;
        let mut new_q = Vec::new();

        for (r_start, r_end) in q {
            let before = (r_start, r_end.min(start));
            let overlap = (r_start.max(start), r_end.min(end));
            let after = (r_start.max(end), r_end);

            if before.0 < before.1 {
                new_q.push(before);
            }
            if overlap.0 < overlap.1 {
                result.push((overlap.0 + dest - start, overlap.1 + dest - start));
            }
            if after.0 < after.1 {
                new_q.push(after);
            }
        }
        q = new_q;
    }

    for x in q {
        result.push(x);
    }

    result
}

/// Parser for 2023 Day 05 (`if_you_give_a_seed_a_fertilizer`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut parts = input.trim().split("\n\n");
    let seeds = parts
        .next()
        .unwrap()
        .split_once(' ')
        .unwrap()
        .1
        .split_ascii_whitespace()
        .map(|c| c.parse().unwrap())
        .collect();

    let mut maps = parts.map(|m| {
        m.lines()
            .skip(1)
            .map(|l| {
                let mut nums = l.split_ascii_whitespace().map(|c| c.parse().unwrap());
                (
                    nums.next().unwrap(),
                    nums.next().unwrap(),
                    nums.next().unwrap(),
                )
            })
            .collect()
    });

    (
        seeds,
        [
            maps.next().unwrap(),
            maps.next().unwrap(),
            maps.next().unwrap(),
            maps.next().unwrap(),
            maps.next().unwrap(),
            maps.next().unwrap(),
            maps.next().unwrap(),
        ],
    )
}

/// Solver for part 1 of 2023 Day 05 (`if_you_give_a_seed_a_fertilizer`)
///
/// # Panics
#[must_use]
pub fn part_1((seeds, almanac): &Input) -> usize {
    seeds
        .iter()
        .map(|seed| almanac.iter().fold(*seed, |acc, x| get(acc, x)))
        .min()
        .unwrap()
}

/// Solver for part 2 of 2023 Day 05 (`if_you_give_a_seed_a_fertilizer`)
///
/// # Panics
#[must_use]
pub fn part_2((seeds, almanac): &Input) -> usize {
    let seed_ranges: Vec<_> = (0..seeds.len() / 2)
        .map(|i| (seeds[2 * i], seeds[2 * i] + seeds[2 * i + 1]))
        .collect();

    *almanac
        .iter()
        .fold(seed_ranges, |acc, x| get_range(&acc, x))
        .iter()
        .map(|(x, _)| x)
        .min()
        .unwrap()
}
