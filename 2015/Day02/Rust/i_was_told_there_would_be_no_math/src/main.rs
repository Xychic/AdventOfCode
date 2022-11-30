use std::{fs, time::Instant};

type Input<'a> = Vec<(usize, usize, usize)>;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    let start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1(&input),
        Instant::now() - start
    );
    let start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2(&input),
        Instant::now() - start
    );
}

fn parse(input: &str) -> Input {
    input
        .trim()
        .split('\n')
        .map(|l| {
            let dims: Vec<_> = l.splitn(3, 'x').collect();
            (
                dims[0].parse().unwrap(),
                dims[1].parse().unwrap(),
                dims[2].parse().unwrap(),
            )
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    input
        .iter()
        .map(|(l, w, h)| {
            let sides = [l * w, w * h, h * l];
            2 * sides.iter().sum::<usize>() + sides.iter().min().unwrap()
        })
        .sum()
}

fn part_2(input: &Input) -> usize {
    input
        .iter()
        .map(|(l, w, h)| {
            let sides = [*l, *w, *h];
            2 * (sides.iter().sum::<usize>() - sides.iter().max().unwrap())
                + sides.iter().product::<usize>()
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "2x3x4";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 58);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 34);
    }
}
