type Input = (Vec<usize>, Vec<(Point, Vec<usize>)>);
type Point = (usize, usize);

/// Parser for 2025 Day 12 (`christmas_tree_farm`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut boxes = Vec::new();
    let mut parts = input.trim().split("\n\n");
    let mut part = parts.next();
    let mut trees = None;
    while let Some(p) = part {
        part = parts.next();
        if part.is_none() {
            trees = Some(
                p.lines()
                    .map(|l| {
                        let (size, boxes) = l.split_once(": ").unwrap();
                        let (x, y) = size.split_once('x').unwrap();
                        (
                            (x.parse().unwrap(), y.parse().unwrap()),
                            boxes.split(' ').map(|x| x.parse().unwrap()).collect(),
                        )
                    })
                    .collect::<Vec<_>>(),
            );
        } else {
            let mut box_parts = p.lines();
            box_parts.next();
            boxes.push(
                box_parts
                    .map(|l| l.chars().filter(|&c| c == '#').count())
                    .sum::<usize>(),
            );
        }
    }

    (boxes, trees.unwrap())
}

/// Solver for part 1 of 2025 Day 12 (`christmas_tree_farm`)
///
/// # Panics
#[must_use]
pub fn part_1((boxes, trees): &Input) -> usize {
    trees
        .iter()
        .filter(|((x, y), bs)| {
            x * y * 10
                > bs.iter()
                    .enumerate()
                    .map(|(i, count)| boxes[i] * count)
                    .sum::<usize>()
                    * 12
        })
        .count()
}
