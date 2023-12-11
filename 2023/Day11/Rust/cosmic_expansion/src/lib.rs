type Input<'a> = (Vec<Vec<char>>, Vec<usize>, Vec<usize>);

/// Parser for 2023 Day 11 (`cosmic_expansion`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let input: Vec<Vec<_>> = input.trim().lines().map(|l| l.chars().collect()).collect();
    let blank_cols: Vec<_> = (0..input[0].len())
        .filter(|x| input.iter().all(|l| l[*x] == '.'))
        .collect();
    let blank_rows: Vec<_> = input
        .iter()
        .enumerate()
        .filter_map(|(y, l)| {
            if l.iter().all(|&c| c == '.') {
                Some(y)
            } else {
                None
            }
        })
        .collect();

    (input, blank_cols, blank_rows)
}

/// Solver for part 1 of 2023 Day 11 (`cosmic_expansion`)
///
/// # Panics
#[must_use]
pub fn part_1((input, blank_cols, blank_rows): &Input) -> usize {
    let galaxies: Vec<(usize, usize)> = input
        .iter()
        .enumerate()
        .flat_map(|(y, l)| {
            l.iter().enumerate().filter_map(move |(x, &c)| {
                if c == '#' {
                    Some((
                        x + blank_cols.iter().filter(|&&col| col < x).count(),
                        y + blank_rows.iter().filter(|&&row| row < y).count(),
                    ))
                } else {
                    None
                }
            })
        })
        .collect();

    galaxies
        .iter()
        .enumerate()
        .flat_map(|(y, g)| std::iter::repeat(g).zip(galaxies.iter().skip(y + 1)))
        .map(|(&(a_x, a_y), &(b_x, b_y))| a_x.abs_diff(b_x) + a_y.abs_diff(b_y))
        .sum()
}

/// Solver for part 2 of 2023 Day 11 (`cosmic_expansion`)
///
/// # Panics
#[must_use]
pub fn part_2((input, blank_cols, blank_rows): &Input) -> usize {
    let galaxies: Vec<(usize, usize)> = input
        .iter()
        .enumerate()
        .flat_map(|(y, l)| {
            l.iter().enumerate().filter_map(move |(x, &c)| {
                if c == '#' {
                    Some((
                        x + 999_999 * blank_cols.iter().filter(|&&col| col < x).count(),
                        y + 999_999 * blank_rows.iter().filter(|&&row| row < y).count(),
                    ))
                } else {
                    None
                }
            })
        })
        .collect();

    galaxies
        .iter()
        .enumerate()
        .flat_map(|(y, g)| std::iter::repeat(g).zip(galaxies.iter().skip(y + 1)))
        .map(|(&(a_x, a_y), &(b_x, b_y))| a_x.abs_diff(b_x) + a_y.abs_diff(b_y))
        .sum()
}
