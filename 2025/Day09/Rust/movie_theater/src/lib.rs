type Input = Vec<Point>;
type Point = (usize, usize);

/// Parser for 2025 Day 09 (`movie_theater`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let (a, b) = l.split_once(',').unwrap();
            (a.parse().unwrap(), b.parse().unwrap())
        })
        .collect()
}

/// Solver for part 1 of 2025 Day 09 (`movie_theater`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .enumerate()
        .flat_map(|(i, (x1, y1))| {
            input
                .iter()
                .skip(i + 2)
                .map(|(x2, y2)| (x1.abs_diff(*x2) + 1) * (y1.abs_diff(*y2) + 1))
        })
        .max()
        .unwrap()
}

/// Solver for part 2 of 2025 Day 09 (`movie_theater`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut possible: Vec<_> = input
        .iter()
        .take(input.len())
        .enumerate()
        .map(|(i, p1 @ (x1, y1))| {
            let p2 @ (x2, y2) = &input[(i + 2) % input.len()];
            (p1, p2, (x1.abs_diff(*x2) + 1) * (y1.abs_diff(*y2) + 1))
        })
        .collect();

    let lines: Vec<_> = input
        .iter()
        .enumerate()
        .map(|(i, p1)| (p1, &input[(i + 1) % input.len()]))
        .collect();

    possible.sort_by_cached_key(|(_, _, x)| usize::MAX - *x);

    possible
        .iter()
        .find(|(&(x1, y1), &(x2, y2), _)| {
            lines.iter().all(|(&(lx1, ly1), &(lx2, ly2))| {
                x1.max(x2) <= lx1.min(lx2)
                    || lx1.max(lx2) <= x1.min(x2)
                    || y1.max(y2) <= ly1.min(ly2)
                    || ly1.max(ly2) <= y1.min(y2)
            })
        })
        .unwrap()
        .2
}
