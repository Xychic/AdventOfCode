type Input<'a> = Vec<(usize, usize, &'a str)>;

const RIGHT: usize = 0;
const DOWN: usize = 1;
const LEFT: usize = 2;
const UP: usize = 3;

/// Parser for 2023 Day 18 (`lavaduct_lagoon`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let mut parts = l.split_ascii_whitespace();
            (
                match parts.next().unwrap() {
                    "R" => RIGHT,
                    "D" => DOWN,
                    "L" => LEFT,
                    "U" => UP,
                    _ => unreachable!(),
                },
                parts.next().unwrap().parse().unwrap(),
                &parts.next().unwrap()[2..8],
            )
        })
        .collect()
}

/// Solver for part 1 of 2023 Day 18 (`lavaduct_lagoon`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut x = 0;
    let mut y = 0;
    let mut ans = 0;

    for &(dir, amt, _) in input {
        let n_x = [x + amt, x, x - amt, x][dir];
        let n_y = [y, y + amt, y, y - amt][dir];

        ans += amt;
        ans += x * n_y;
        ans -= n_x * y;

        x = n_x;
        y = n_y;
    }
    ans / 2 + 1
}

/// Solver for part 2 of 2023 Day 18 (`lavaduct_lagoon`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut x = 0;
    let mut y = 0;
    let mut ans = 0;

    for &(_, _, ins) in input {
        let dir: usize = ins[5..6].parse().unwrap();
        let amt = usize::from_str_radix(&ins[..5], 16).unwrap();
        let n_x = [x + amt, x, x - amt, x][dir];
        let n_y = [y, y + amt, y, y - amt][dir];

        ans += amt;
        ans += x * n_y;
        ans -= n_x * y;

        x = n_x;
        y = n_y;
    }
    ans / 2 + 1
}
