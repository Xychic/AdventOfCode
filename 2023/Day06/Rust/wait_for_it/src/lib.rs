type Input<'a> = Vec<(&'a str, &'a str)>;

/// Parser for 2023 Day 06 (`wait_for_it`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let (a, b) = input.trim().split_once('\n').unwrap();
    a.split_ascii_whitespace()
        .zip(b.split_ascii_whitespace())
        .skip(1)
        .collect()
}

/// Solver for part 1 of 2023 Day 06 (`wait_for_it`)
///
/// # Panics
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .map(|&(time, dist)| {
            let b: f32 = time.parse().unwrap();
            let c: f32 = dist.parse().unwrap();

            let discriminant = (b * b - 4. * c).sqrt();
            
            // Need to add slight offset if there is no fractional part
            let upper = ((b + discriminant) / 2. - 0.0001).floor();
            let lower = ((b - discriminant) / 2. + 0.0001).ceil();
            let test = upper - lower;

            test as usize + 1

            // dbg!(test, upper - lower + 1);
        })
        .product()
}

/// Solver for part 2 of 2023 Day 06 (`wait_for_it`)
///
/// # Panics
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let (time, dist) = input.iter().fold(
        (String::new(), String::new()),
        |(acc_a, acc_b), (x_a, x_b)| (acc_a + x_a, acc_b + x_b),
    );
    let b: f32 = time.parse().unwrap();
    let c: f32 = dist.parse().unwrap();

    let discriminant = (b * b - 4. * c).sqrt();

    // Need to add slight offset if there is no fractional part
    let upper = ((b + discriminant) / 2. - 0.0001).floor();
    let lower = ((b - discriminant) / 2. + 0.0001).ceil();
    let test = upper - lower;

    test as usize + 1
}
