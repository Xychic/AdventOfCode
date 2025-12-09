use std::collections::HashMap;

type Input = (Vec<Point>, Vec<(usize, usize)>);
type Point = (usize, usize, usize);

/// Parser for 2025 Day 08 (`playground`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let points: Vec<Point> = input
        .trim()
        .lines()
        .map(|l| {
            let mut iter = l.splitn(3, ',').map(|s| s.parse::<usize>().unwrap());
            (
                iter.next().unwrap(),
                iter.next().unwrap(),
                iter.next().unwrap(),
            )
        })
        .collect();

    let mut pairs: Vec<(usize, usize)> = points
        .iter()
        .enumerate()
        .flat_map(|(i, _)| {
            points
                .iter()
                .enumerate()
                .skip(i + 1)
                .map(move |(j, _)| (i, j))
        })
        .collect();
    pairs.sort_by_cached_key(|(i, j)| {
        let (x1, y1, z1) = points[*i];
        let (x2, y2, z2) = points[*j];
        (x1.abs_diff(x2)).pow(2) + (y1.abs_diff(y2)).pow(2) + (z1.abs_diff(z2)).pow(2)
    });
    (points, pairs)
}

fn get_parent(id: usize, parents: &mut HashMap<usize, usize>) -> usize {
    let current_parent = *parents.get(&id).unwrap();
    if id == current_parent {
        return id;
    }
    let new_parent = get_parent(current_parent, parents);
    parents.insert(id, new_parent);
    new_parent
}

/// Solver for part 1 of 2025 Day 08 (`playground`)
///
/// # Panics
#[must_use]
pub fn part_1<const CONNECTIONS: usize>((points, pairs): &Input) -> usize {
    let mut parents: HashMap<_, _> = (0..points.len()).map(|i| (i, i)).collect();

    for &(a, b) in pairs.iter().take(CONNECTIONS) {
        let parent_a = get_parent(a, &mut parents);
        let parent_b = get_parent(b, &mut parents);
        if parent_a != parent_b {
            parents.insert(parent_a, parent_b);
        }
    }

    println!("{parents:?}");
    let mut sizes = HashMap::new();
    for i in 0..points.len() {
        *sizes.entry(get_parent(i, &mut parents)).or_insert(0) += 1;
    }
    let mut sizes: Vec<_> = sizes.values().collect();
    sizes.sort_by(|a, b| b.cmp(a));
    println!("{sizes:?}");
    // sizes[0] * sizes[1] * sizes[2]
    0
}

/// Solver for part 2 of 2025 Day 08 (`playground`)
///
/// # Panics
#[must_use]
pub fn part_2((points, pairs): &Input) -> usize {
    let mut parents: HashMap<_, _> = (0..points.len()).map(|i| (i, i)).collect();
    let mut last = &pairs[0];
    for p @ &(a, b) in pairs {
        let parent_a = get_parent(a, &mut parents);
        let parent_b = get_parent(b, &mut parents);
        if parent_a != parent_b {
            parents.insert(parent_a, parent_b);
            last = p;
        }
    }
    points[last.0].0 * points[last.1].0
}
