#[derive(Debug, Clone, Copy)]
pub enum Path {
    Move(usize),
    Left,
    Right,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Tile {
    Open,
    Wall,
    Null,
}

type Input = (Vec<Vec<Tile>>, Vec<Path>);

pub fn parse(input: &str) -> Input {
    let (map, ins) = input.trim_end().split_once("\n\n").unwrap();

    let mut map: Vec<Vec<_>> = map
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| match c {
                    '#' => Tile::Wall,
                    '.' => Tile::Open,
                    ' ' => Tile::Null,
                    _ => unreachable!(),
                })
                .collect()
        })
        .collect();

    let width = map.iter().map(|row| row.len()).max().unwrap();
    for row in &mut map {
        row.resize(width, Tile::Null);
    }

    let mut path = Vec::with_capacity(ins.len());
    let mut collector = String::new();

    for chr in ins.chars() {
        match chr {
            'L' | 'R' => {
                if !collector.is_empty() {
                    path.push(Path::Move(collector.parse().unwrap()));
                    collector = String::new();
                }
                path.push(if chr == 'L' { Path::Left } else { Path::Right });
            }
            x => collector += &x.to_string(),
        }
    }
    if !collector.is_empty() {
        path.push(Path::Move(collector.parse().unwrap()));
    }
    (map, path)
}

#[allow(dead_code)]
fn show(grid: &[Vec<Tile>], pos: (usize, usize)) {
    for (y, row) in grid.iter().enumerate() {
        let mut row_string = String::with_capacity(row.len());
        for (x, tile) in row.iter().enumerate() {
            row_string += if (x, y) == pos {
                "X"
            } else {
                match tile {
                    Tile::Open => ".",
                    Tile::Wall => "#",
                    Tile::Null => " ",
                }
            };
        }
        println!("{row_string}");
    }
    println!()
}

const DELTA: [(isize, isize); 4] = [(1, 0), (0, 1), (-1, 0), (0, -1)];

pub fn part_1((grid, path): &Input) -> usize {
    let width = grid[0].len() - 1;
    let height = grid.len() - 1;

    let mut y = 0;
    let mut x = grid[0]
        .iter()
        .enumerate()
        .find(|&(_, &t)| t == Tile::Open)
        .unwrap()
        .0;

    let mut d = 0;
    for action in path {
        match action {
            Path::Left => d = (d + 3) % 4,
            Path::Right => d = (d + 1) % 4,
            Path::Move(count) => {
                let (dx, dy) = DELTA[d];
                for _ in 0..*count {
                    let (new_x, new_y) = (
                        width.min(((x as isize + dx) % width as isize) as usize),
                        height.min(((y as isize + dy) % height as isize) as usize),
                    );
                    match grid[new_y][new_x] {
                        Tile::Open => {
                            x = new_x;
                            y = new_y;
                        }
                        Tile::Wall => {
                            break;
                        }
                        Tile::Null => {
                            if d == 0 {
                                let (new_x, tile) = grid[y]
                                    .iter()
                                    .enumerate()
                                    .find(|&(_, &t)| t != Tile::Null)
                                    .unwrap();
                                if tile == &Tile::Wall {
                                    break;
                                }
                                x = new_x;
                            } else if d == 1 {
                                let (new_y, tile) = grid
                                    .iter()
                                    .enumerate()
                                    .find(|&(_, row)| row[x] != Tile::Null)
                                    .unwrap();

                                if tile[x] == Tile::Wall {
                                    break;
                                }
                                y = new_y;
                            } else if d == 2 {
                                let (new_x, tile) = grid[y]
                                    .iter()
                                    .enumerate()
                                    .rev()
                                    .find(|&(_, &t)| t != Tile::Null)
                                    .unwrap();
                                if tile == &Tile::Wall {
                                    break;
                                }
                                x = new_x;
                            } else {
                                let (new_y, tile) = grid
                                    .iter()
                                    .enumerate()
                                    .rev()
                                    .find(|&(_, row)| row[x] != Tile::Null)
                                    .unwrap();
                                if tile[x] == Tile::Wall {
                                    break;
                                }
                                y = new_y;
                            }
                        }
                    }
                }
            }
        }
    }
    1000 * (y + 1) + 4 * (x + 1) + d
}

const REGIONS: [(usize, usize); 6] = [(1, 0), (2, 0), (1, 1), (1, 2), (0, 2), (0, 3)];

fn global_to_regional(
    (x, y): &(usize, usize),
    region_index: usize,
    side_len: usize,
) -> (usize, usize) {
    let (region_x, region_y) = REGIONS[region_index];
    (x - region_x * side_len, y - region_y * side_len)
}

fn regional_to_global(
    (x, y): &(usize, usize),
    region_index: usize,
    side_len: usize,
) -> (usize, usize) {
    let (region_x, region_y) = REGIONS[region_index];
    (x + region_x * side_len, y + region_y * side_len)
}

fn get_region(pos: &(usize, usize), side_len: usize) -> usize {
    let &(x, y) = pos;
    for (index, &(region_x, region_y)) in REGIONS.iter().enumerate() {
        if region_x * side_len <= x
            && x < (region_x + 1) * side_len
            && region_y * side_len <= y
            && y < (region_y + 1) * side_len
        {
            return index;
        }
    }
    unreachable!()
}

fn wrap_pos_around(
    &(x, y): &(usize, usize),
    d: usize,
    new_d: usize,
    side_len: usize,
) -> (usize, usize) {
    let val = match d {
        0 => y,
        1 => side_len - 1 - x,
        2 => side_len - 1 - y,
        3 => x,
        _ => unreachable!(),
    };

    match new_d {
        0 => (0, val),
        1 => (side_len - 1 - val, 0),
        2 => (side_len - 1, side_len - 1 - val),
        3 => (val, side_len - 1),
        _ => unreachable!(),
    }
}

fn translate_region_dir(region: usize, dir: usize) -> (usize, usize) {
    match (region, dir) {
        (0, 0) => (1, 0),
        (0, 1) => (2, 1),
        (0, 2) => (4, 0),
        (0, 3) => (5, 0),
        (1, 0) => (3, 2),
        (1, 1) => (2, 2),
        (1, 2) => (0, 2),
        (1, 3) => (5, 3),
        (2, 0) => (1, 3),
        (2, 1) => (3, 1),
        (2, 2) => (4, 1),
        (2, 3) => (0, 3),
        (3, 0) => (1, 2),
        (3, 1) => (5, 2),
        (3, 2) => (4, 2),
        (3, 3) => (2, 3),
        (4, 0) => (3, 0),
        (4, 1) => (5, 1),
        (4, 2) => (0, 0),
        (4, 3) => (2, 0),
        (5, 0) => (3, 3),
        (5, 1) => (1, 1),
        (5, 2) => (0, 1),
        (5, 3) => (4, 3),
        _ => unreachable!(),
    }
}

fn find_new_region_pos(pos: &(usize, usize), d: usize, side_len: usize) -> ((usize, usize), usize) {
    let region_index = get_region(pos, side_len);
    let (new_region_index, new_dir) = translate_region_dir(region_index, d);
    (
        regional_to_global(
            &wrap_pos_around(
                &global_to_regional(pos, region_index, side_len),
                d,
                new_dir,
                side_len,
            ),
            new_region_index,
            side_len,
        ),
        new_dir,
    )
}

pub fn part_2((grid, path): &Input) -> usize {
    let width = grid[0].len();
    let height = grid.len();
    let side_len = width / 3;

    let mut y = 0;
    let mut x = grid[0]
        .iter()
        .enumerate()
        .find(|&(_, &t)| t == Tile::Open)
        .unwrap()
        .0;
    let mut d = 0;
    for action in path {
        match action {
            Path::Left => d = (d + 3) % 4,
            Path::Right => d = (d + 1) % 4,
            Path::Move(count) => {
                let (mut dx, mut dy) = DELTA[d];
                for _ in 0..*count {
                    let (new_x, new_y) = (
                        (width - 1).min(((x as isize + dx) % width as isize) as usize),
                        (height - 1).min(((y as isize + dy) % height as isize) as usize),
                    );
                    match grid[new_y][new_x] {
                        Tile::Open => {
                            x = new_x;
                            y = new_y;
                        }
                        Tile::Wall => {
                            break;
                        }
                        Tile::Null => {
                            let ((new_x, new_y), new_d) = find_new_region_pos(&(x, y), d, side_len);
                            if grid[new_y][new_x] == Tile::Wall {
                                break;
                            }
                            x = new_x;
                            y = new_y;
                            d = new_d;
                            (dx, dy) = DELTA[d];
                        }
                    }
                }
            }
        }
    }

    1000 * (y + 1) + 4 * (x + 1) + d
}
