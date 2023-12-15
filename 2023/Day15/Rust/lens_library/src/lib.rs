type Input<'a> = Vec<Vec<char>>;

/// Parser for 2023 Day 15 (`lens_library`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .split(',')
        .map(|s| s.chars().collect())
        .collect()
}

/// Solver for part 1 of 2023 Day 15 (`lens_library`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .map(|xs| xs.iter().fold(0, |acc, &x| ((acc + x as usize) * 17) % 256))
        .sum()
}

/// Solver for part 2 of 2023 Day 15 (`lens_library`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let mut boxes = vec![Vec::new(); 256];
    for xs in input {
        match xs.last().unwrap() {
            '-' => {
                let label: String = xs[..xs.len() - 1].iter().collect();
                let index = label
                    .chars()
                    .fold(0, |acc, x| ((acc + x as usize) * 17) % 256);
                match boxes[index]
                    .iter()
                    .enumerate()
                    .find(|(_, (l, _))| *l == label)
                {
                    None => continue,
                    Some((c, _)) => {
                        boxes.get_mut(index).unwrap().remove(c);
                    }
                }
            }
            c => {
                let val = c.to_digit(10).unwrap() as usize;
                let label: String = xs[..xs.len() - 2].iter().collect();
                let index = label
                    .chars()
                    .fold(0, |acc, x| ((acc + x as usize) * 17) % 256);

                match boxes[index]
                    .iter()
                    .enumerate()
                    .find(|(_, (l, _))| *l == label)
                {
                    None => {
                        boxes.get_mut(index).unwrap().push((label, val));
                    }
                    Some((c, _)) => {
                        let box_ = boxes.get_mut(index).unwrap();
                        box_.insert(c, (label, val));
                        box_.remove(c + 1);
                    }
                }
            }
        }
    }

    boxes
        .iter()
        .enumerate()
        .map(|(i, box_)| {
            (i + 1)
                * box_
                    .iter()
                    .enumerate()
                    .map(|(j, (_, x))| (j + 1) * x)
                    .sum::<usize>()
        })
        .sum()
}
