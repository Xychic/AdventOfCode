use std::collections::{HashMap, HashSet, VecDeque};

type Point = (usize, usize);
type Grid = Vec<Vec<char>>;
type Map = HashMap<Point, (char, HashSet<(Point, usize)>)>;
type Input<'a> = (Point, Point, Map);

fn make_grid(input: &str) -> (Grid, Point, Point, HashSet<Point>) {
    let grid: Grid = input.trim().lines().map(|l| l.chars().collect()).collect();

    let mut start = (0, 0);
    let mut end = (0, 0);
    let mut pois = HashSet::new();

    for (y, row) in grid.iter().enumerate() {
        for (x, &c) in row.iter().enumerate() {
            if y == 0 && c == '.' {
                start = (x, y);
            }
            if y == grid.len() - 1 && c == '.' {
                end = (x, y);
            }
            if c != '.'
                || [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
                    .iter()
                    .filter(|(nx, ny)| {
                        *nx < grid[y].len() && *ny < grid.len() && grid[*ny][*nx] != '#'
                    })
                    .count()
                    > 2
            {
                pois.insert((x, y));
            }
        }
    }
    (grid, start, end, pois)
}

/// Parser for 2023 Day 23 (`a_long_walk`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let (grid, start, end, pois) = make_grid(input);

    let mut map = HashMap::new();
    let mut queue = VecDeque::new();
    let mut seen_points = HashSet::new();
    queue.push_back(start);
    seen_points.insert(start);
    while let Some(pos @ (x, y)) = queue.pop_front() {
        let c = grid[y][x];
        let possible = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)];
        let possible: Vec<_> = possible
            .iter()
            .filter(|(nx, ny)| *nx < grid[0].len() && *ny < grid.len() && grid[*ny][*nx] != '#')
            .collect();
        if pois.contains(&pos) {
            let mut neighbours = HashSet::with_capacity(4);
            for &npos in possible {
                neighbours.insert((npos, 1));
                if !seen_points.contains(&npos) {
                    queue.push_back(npos);
                    seen_points.insert(npos);
                }
            }
            map.insert(pos, (c, neighbours));
            continue;
        }

        // Not POI, need to compress
        let mut dist = 0;
        let mut current = pos;
        let mut run_seen = HashSet::new();
        run_seen.insert(pos);
        for n in possible {
            if pois.contains(n) {
                map.entry(pos)
                    .or_insert((c, HashSet::new()))
                    .1
                    .insert((*n, 1));
            }
        }
        loop {
            let (cx, cy) = current;
            let possible = [(cx - 1, cy), (cx + 1, cy), (cx, cy - 1), (cx, cy + 1)];

            let neighbours: Vec<_> = possible
                .iter()
                .filter(|npos @ (nx, ny)| {
                    !run_seen.contains(npos)
                        && *nx < grid[0].len()
                        && *ny < grid.len()
                        && grid[*ny][*nx] != '#'
                })
                .collect();
            let non_poi_count = neighbours
                .iter()
                .filter(|npos| !pois.contains(npos))
                .count();
            if non_poi_count == 0 {
                map.entry(pos)
                    .or_insert((c, HashSet::new()))
                    .1
                    .insert((current, dist));
                if neighbours.len() == 1 {
                    map.insert(
                        current,
                        (
                            grid[cy][cx],
                            HashSet::from([(*neighbours[0], 1), (pos, dist)]),
                        ),
                    );
                    seen_points.insert(current);
                    if !seen_points.contains(neighbours[0]) {
                        queue.push_back(*neighbours[0]);
                        seen_points.insert(*neighbours[0]);
                    }
                }
                break;
            }
            assert!(non_poi_count == 1);
            current = **neighbours.iter().find(|&&n| !pois.contains(n)).unwrap();
            run_seen.insert(current);
            dist += 1;
        }
    }

    (start, end, map)
}

fn dfs(
    pos: &Point,
    target: &Point,
    dist: usize,
    seen: &mut HashSet<Point>,
    map: &Map,
    validator: &impl Fn(&char, &Point, &Point) -> bool,
) -> usize {
    if seen.contains(pos) {
        0
    } else if pos == target {
        dist
    } else {
        let mut max_cost = 0;
        let (c, possible) = map.get(pos).unwrap();
        seen.insert(*pos);
        for &(npos, cost) in possible {
            let valid = validator(c, pos, &npos);
            if !valid {
                continue;
            }
            max_cost = max_cost.max(dfs(&npos, target, dist + cost, seen, map, validator));
        }
        seen.remove(pos);
        max_cost
    }
}

/// Solver for part 1 of 2023 Day 23 (`a_long_walk`)
///
/// # Panics
#[must_use]
pub fn part_1((start, end, map): &Input) -> usize {
    dfs(
        start,
        end,
        0,
        &mut HashSet::new(),
        map,
        &|c, &(x, y), &(nx, ny)| match *c {
            '.' => true,
            '^' => ny == (y - 1),
            '>' => nx == (x + 1),
            'v' => ny == (y + 1),
            '<' => nx == (x - 1),
            _ => unreachable!("Invalid c: {c}"),
        },
    )
}

/// Solver for part 2 of 2023 Day 23 (`a_long_walk`)
///
/// # Panics
#[must_use]
pub fn part_2((start, end, map): &Input) -> usize {
    dfs(start, end, 0, &mut HashSet::new(), map, &|_, _, _| true)
}
