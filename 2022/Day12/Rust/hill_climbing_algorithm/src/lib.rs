use std::collections::{HashSet, VecDeque};

type Point = (usize, usize);
type Input<'a> = (Point, Point, Vec<Vec<usize>>);

pub fn parse(input: &str) -> Input {
    let mut start = (usize::MAX, usize::MAX);
    let mut end = (usize::MAX, usize::MAX);
    let points = input
        .trim()
        .lines()
        .enumerate()
        .map(|(y, l)| {
            l.chars()
                .enumerate()
                .map(|(x, chr)| {
                    (match chr {
                        'S' => {
                            start = (x, y);
                            0
                        }
                        'E' => {
                            end = (x, y);
                            25
                        }
                        c => (c as u32) - ('a' as u32),
                    } as usize)
                })
                .collect()
        })
        .collect();
    assert!(start != (usize::MAX, usize::MAX));
    assert!(end != (usize::MAX, usize::MAX));

    (start, end, points)
}

pub fn part_1((start, end, grid): &Input) -> usize {
    let mut queue = VecDeque::new();
    queue.push_back((*start, 0));
    search(&mut queue, end, grid)
}

pub fn part_2((_, end, grid): &Input) -> usize {
    let mut queue = VecDeque::new();
    for (y, row) in grid.iter().enumerate() {
        for (x, &height) in row.iter().enumerate() {
            if height == 0 {
                queue.push_back(((x, y), 0));
            }
        }
    }

    search(&mut queue, end, grid)
}

fn search(queue: &mut VecDeque<(Point, usize)>, end: &Point, grid: &Vec<Vec<usize>>) -> usize {
    let row_len = grid[0].len();
    let col_len = grid.len();

    let mut seen = HashSet::<Point>::with_capacity(grid.len() * grid[0].len());
    while let Some((p @ (x, y), d)) = queue.pop_front() {
        if seen.contains(&p) {
            continue;
        }
        seen.insert(p);

        if p == *end {
            return d;
        }

        let height = grid[y][x];

        for (dx, dy) in [(0, 1), (1, 0), (0, -1), (-1, 0)] {
            let new_x = (x as isize + dx) as usize;
            let new_y = (y as isize + dy) as usize;

            if new_x < row_len && new_y < col_len {
                let new_height = grid[new_y][new_x];
                if new_height <= height + 1 {
                    queue.push_back(((new_x, new_y), d + 1))
                }
            }
        }
    }
    unreachable!()
}
