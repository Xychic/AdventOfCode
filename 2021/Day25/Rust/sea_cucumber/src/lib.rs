type Input<'a> = Vec<Vec<Cell>>;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Cell {
    East,
    South,
    Space,
}

pub fn parse(input: &str) -> Input {
    input
        .trim()
        .split('\n')
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    '>' => Cell::East,
                    'v' => Cell::South,
                    '.' => Cell::Space,
                    _ => unreachable!(),
                })
                .collect()
        })
        .collect()
}

pub fn part_1(input: &Input) -> usize {
    let mut data = input.clone();
    let mut to_change;

    let row_len = data[0].len();
    let col_len = data.len();

    for i in 1.. {
        let mut changed = false;
        to_change = Vec::new();
        for (y, row) in data.iter().enumerate() {
            for (x, cell) in row.iter().enumerate() {
                match cell {
                    Cell::East => {
                        if data[y][(x + 1) % row_len] == Cell::Space {
                            to_change.push((((x + 1) % row_len, y), Cell::East));
                            to_change.push(((x, y), Cell::Space));
                        }
                    }
                    _ => continue,
                }
            }
        }
        if !to_change.is_empty() {
            changed = true;
        }
        for ((x, y), new) in to_change {
            data[y][x] = new;
        }

        to_change = Vec::new();
        for (y, row) in data.iter().enumerate() {
            for (x, cell) in row.iter().enumerate() {
                match cell {
                    Cell::South => {
                        if data[(y + 1) % col_len][x] == Cell::Space {
                            to_change.push(((x, (y + 1) % col_len), Cell::South));
                            to_change.push(((x, y), Cell::Space));
                        }
                    }
                    _ => continue,
                }
            }
        }
        if !to_change.is_empty() {
            changed = true;
        }
        for ((x, y), new) in to_change {
            data[y][x] = new;
        }

        if !changed {
            return i;
        }
    }
    unreachable!()
}
