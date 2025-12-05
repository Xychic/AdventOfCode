type Input = (Vec<Point>, Vec<usize>);
type Point = (usize, usize);

/// Parser for 2025 Day 05 (`cafeteria`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let (fresh, ingredients) = input.trim().split_once("\n\n").unwrap();
    (
        fresh
            .lines()
            .map(|l| {
                let (low, high) = l.split_once('-').unwrap();
                (low.parse().unwrap(), high.parse().unwrap())
            })
            .collect(),
        ingredients.lines().map(|x| x.parse().unwrap()).collect(),
    )
}

/// Solver for part 1 of 2025 Day 05 (`cafeteria`)
///
/// # Panics
#[must_use]
pub fn part_1((fresh, ingredients): &Input) -> usize {
    ingredients
        .iter()
        .filter(|&x| fresh.iter().any(|(low, high)| low <= x && x <= high))
        .count()
}

/// Solver for part 2 of 2025 Day 05 (`cafeteria`)
///
/// # Panics
#[must_use]
pub fn part_2((fresh, _): &Input) -> usize {
    let mut fresh = fresh.to_owned();
    fresh.sort_by(|(a, b), (c, d)| match a.cmp(c) {
        std::cmp::Ordering::Equal => b.cmp(d),
        x => x,
    });
    let mut ans = 0;
    let mut x = 0;
    for (low, high) in &mut fresh {
        if *high <= x {
            continue;
        }
        if *low <= x {
            *low = x + 1;
        }
        ans += 1 + *high - *low;
        x = x.max(*high);
    }

    ans
}
