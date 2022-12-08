type Input<'a> = Vec<Vec<u32>>;

pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| l.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect()
}

#[rustfmt::skip]
pub fn part_1(input: &Input) -> usize {
    let row_len = input[0].len();
    let col_len = input.len();
    let mut visible = row_len * 2 + col_len * 2 - 4;
    for (y, row) in input.iter().enumerate().skip(1) {
        if y == col_len - 1 {
            continue;
        }
        for (x, &tree) in row.iter().enumerate().skip(1) {
            if x == row_len - 1 {
                continue;
            }
            if (0..x).all(|x2| input[y][x2] < tree)                 // UP
                || (x + 1..row_len).all(|x2| input[y][x2] < tree)   // DOWN
                || (0..y).all(|y2| input[y2][x] < tree)             // LEFT
                || (y + 1..col_len).all(|y2| input[y2][x] < tree)   // RIGHT
            {
                visible += 1;
            }
        }
    }
    visible
}

pub fn part_2(input: &Input) -> usize {
    let row_len = input[0].len();
    let col_len = input.len();

    input
        .iter()
        .enumerate()
        .map(|(y, row)| {
            row.iter()
                .enumerate()
                .map(|(x, &tree)| {
                    let mut left = (0..x).rev().take_while(|&x2| input[y][x2] < tree).count();
                    if left < x.saturating_sub(1) {
                        left += 1
                    }
                    let mut right = (x + 1..row_len)
                        .take_while(|&x2| input[y][x2] < tree)
                        .count();
                    if right < row_len - 1 - x {
                        right += 1;
                    }
                    let mut up = (0..y).rev().take_while(|&y2| input[y2][x] < tree).count();
                    if up < y.saturating_sub(1) {
                        up += 1;
                    }
                    let mut down = (y + 1..col_len)
                        .take_while(|&y2| input[y2][x] < tree)
                        .count();

                    if down < col_len - 1 - y {
                        down += 1;
                    }
                    up * down * left * right
                })
                .max()
                .unwrap()
        })
        .max()
        .unwrap()
}
