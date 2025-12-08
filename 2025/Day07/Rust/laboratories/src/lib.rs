type Input = Vec<Vec<char>>;

/// Parser for 2025 Day 07 (`laboratories`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input.trim().lines().map(|l| l.chars().collect()).collect()
}

/// Solver for part 1 of 2025 Day 07 (`laboratories`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let mut boards: Vec<_> = input[0].iter().map(|&c| c == 'S').collect();
    let mut ans = 0;
    for line in input {
        for (i, &c) in line.iter().enumerate() {
            if boards[i] && c == '^' {
                ans += 1;
                if let Some(b) = boards.get_mut(i - 1) {
                    *b = true;
                }
                if let Some(b) = boards.get_mut(i + 1) {
                    *b = true;
                }
                boards[i] = false;
            }
        }
    }
    ans
}

/// Solver for part 2 of 2025 Day 07 (`laboratories`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut boards: Vec<_> = input[0].iter().map(|&c| usize::from(c == 'S')).collect();
    for line in input {
        for (i, &c) in line.iter().enumerate().skip(1).take(boards.len() - 2) {
            if boards[i] != 0 && c == '^' {
                boards[i - 1] += boards[i];
                boards[i + 1] += boards[i];
                boards[i] = 0;
            }
        }
    }
    boards.iter().sum()
}
