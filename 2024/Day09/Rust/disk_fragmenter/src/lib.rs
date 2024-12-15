// type Input<'a> = (Vec<Option<usize>>, Vec<(usize, usize)>);
type Input<'a> = (Vec<(usize, usize, usize)>, Vec<(usize, usize)>);

/// Parser for 2024 Day 09 (`disk_fragmenter`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut label = 0;
    let mut index = 0;
    let mut files = Vec::new();
    let mut spaces = Vec::new();

    let mut is_file = true;

    for size in input
        .trim()
        .chars()
        .map(|c| c.to_digit(10).unwrap() as usize)
    {
        if is_file {
            files.push((label, index, size));
            label += 1;
        } else {
            spaces.push((index, size));
        }
        index += size;
        is_file = !is_file;
    }
    (files, spaces)
}

/// Solver for part 1 of 2024 Day 09 (`disk_fragmenter`)
///
/// # Panics
#[must_use]
pub fn part_1((files, spaces): &Input) -> usize {
    let result_size = files
        .iter()
        .map(|(_, _, x)| x)
        .chain(spaces.iter().map(|(_, x)| x))
        .sum();
    let mut result = vec![None; result_size];
    let mut spaces = spaces.to_owned();

    for &(label, index, size) in files.iter().rev() {
        for i in (0..size).rev() {
            result[index + i] = Some(label);
            for (space_index, space_size) in &mut spaces {
                if *space_index < index + i && *space_size > 0 {
                    result[index + i] = None;
                    result[*space_index] = Some(label);
                    *space_index += 1;
                    *space_size -= 1;
                    break;
                }
            }
        }
    }

    result
        .iter()
        .enumerate()
        .map(|(i, x)| i * x.unwrap_or(0))
        .sum()
}

/// Solver for part 2 of 2024 Day 09 (`disk_fragmenter`)
///
/// # Panics
#[must_use]
pub fn part_2((files, spaces): &Input) -> usize {
    let result_size = files
        .iter()
        .map(|(_, _, x)| x)
        .chain(spaces.iter().map(|(_, x)| x))
        .sum();
    let mut result = vec![None; result_size];
    let mut spaces = spaces.to_owned();

    for &(id, index, size) in files.iter().rev() {
        for i in 0..size {
            result[index + i] = Some(id);
        }
        for (space_index, space_size) in &mut spaces {
            if *space_index < index && size <= *space_size {
                for i in 0..size {
                    result[index + i] = None;
                    result[*space_index + i] = Some(id);
                }
                *space_index += size;
                *space_size -= size;
                break;
            }
        }
    }

    result
        .iter()
        .enumerate()
        .map(|(i, x)| i * x.unwrap_or(0))
        .sum()
}
