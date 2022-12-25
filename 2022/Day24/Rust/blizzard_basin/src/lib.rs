use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Blizzard {
    pos: (isize, isize),
    dir: (isize, isize),
    // symbol: String,
}

impl Blizzard {
    fn step(&mut self, &width: &isize, &height: &isize) {
        let (mut x, mut y) = self.pos;
        let (dx, dy) = self.dir;

        x += dx;
        y += dy;

        if x == width {
            x = 1;
        } else if x == 0 {
            x = width - 1;
        }
        if y == height {
            y = 1;
        } else if y == 0 {
            y = height - 1;
        }
        self.pos = (x, y);
    }
}

type Input = ((isize, isize), Vec<Blizzard>);

pub fn parse(input: &str) -> Input {
    let mut blizzards = Vec::new();
    let mut width = 0;
    let mut height = 0;

    for (y, line) in input.trim().lines().enumerate() {
        height = height.max(y);
        for (x, c) in line.char_indices() {
            width = width.max(x);
            blizzards.push(Blizzard {
                pos: (x as isize, y as isize),
                dir: match c {
                    '<' => (-1, 0),
                    '>' => (1, 0),
                    'v' => (0, 1),
                    '^' => (0, -1),
                    _ => continue,
                },
            });
        }
    }
    let width = width as isize;
    let height = height as isize;

    ((width, height), blizzards)
}

pub fn part_1(((width, height), blizzards): &Input) -> usize {
    let mut blizzards = blizzards.to_owned();
    let mut blocked = HashMap::new();

    let mut queue = VecDeque::new();
    let target = (width - 1, *height);
    let mut seen = HashSet::new();

    for b in &mut blizzards {
        b.step(width, height);
    }
    blocked.insert(1, blizzards.iter().map(|b| b.pos).collect::<HashSet<_>>());

    let start = (1, 0);
    queue.push_back((start, 0));
    while let Some(((x, y), d)) = queue.pop_front() {
        if (x, y) == target {
            return d;
        }
        if seen.contains(&(x, y, d)) {
            continue;
        }
        seen.insert((x, y, d));

        let currently_blocked = blocked.entry(d + 1).or_insert_with(|| {
            for b in &mut blizzards {
                b.step(width, height);
            }
            blizzards.iter().map(|b| b.pos).collect::<HashSet<_>>()
        });

        for (dx, dy) in [(0, 1), (0, -1), (1, 0), (-1, 0), (0, 0)] {
            let new_x = x + dx;
            let new_y = y + dy;

            if (new_x <= 0 || new_y <= 0) && (new_x, new_y) != start {
                continue;
            } else if (new_x >= *width || new_y >= *height) && (new_x, new_y) != target {
                continue;
            } else if !currently_blocked.contains(&(new_x, new_y)) {
                queue.push_back(((new_x, new_y), d + 1));
            }
        }
    }
    unreachable!()
}

pub fn part_2(((width, height), blizzards): &Input) -> usize {
    let mut blizzards = blizzards.to_owned();
    let mut blocked = HashMap::new();

    let mut queue = VecDeque::new();
    let target = (width - 1, *height);
    let start = (1, 0);
    let mut seen = HashSet::new();

    for b in &mut blizzards {
        b.step(width, height);
    }
    blocked.insert(1, blizzards.iter().map(|b| b.pos).collect::<HashSet<_>>());

    queue.push_back((start, 0, false, false));

    while let Some(state) = queue.pop_front() {
        let ((x, y), d, mut reached_end, mut reached_start) = state;

        if (x, y) == target && reached_end && reached_start {
            return d;
        } else if (x, y) == start && reached_end {
            reached_start = true;
        } else if (x, y) == target {
            reached_end = true;
        } else if seen.contains(&state) {
            continue;
        }
        seen.insert(state);

        let currently_blocked = blocked.entry(d + 1).or_insert_with(|| {
            for b in &mut blizzards {
                b.step(width, height);
            }
            blizzards.iter().map(|b| b.pos).collect::<HashSet<_>>()
        });

        for (dx, dy) in [(0, 1), (0, -1), (1, 0), (-1, 0), (0, 0)] {
            let new_x = x + dx;
            let new_y = y + dy;
            if (new_x <= 0 || new_y <= 0) && (new_x, new_y) != start {
                continue;
            }
            if (new_x >= *width || new_y >= *height) && (new_x, new_y) != target {
                continue;
            }
            if !currently_blocked.contains(&(new_x, new_y)) {
                queue.push_back(((new_x, new_y), d + 1, reached_end, reached_start));
            }
        }
    }
    unreachable!()
}
