use std::{fs, time::Instant};

type Input = Vec<usize>;

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
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect()
}

fn part_1(input: &Input) -> usize {
    let min = *input.iter().min().unwrap();
    let max = *input.iter().max().unwrap();
    (min..=max)
        .map(|p| input.iter().map(|&x| x.max(p) - x.min(p)).sum())
        .min()
        .unwrap()
}

fn part_2(input: &Input) -> usize {
    let min = *input.iter().min().unwrap();
    let max = *input.iter().max().unwrap();
    (min..=max)
        .map(|p| {
            input
                .iter()
                .map(|&x| {
                    let n = x.max(p) - x.min(p);
                    n * (n + 1) / 2
                })
                .sum()
        })
        .min()
        .unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        let test_input = "16,1,2,0,4,2,7,1,2,14";
        let test_answer = 37;
        assert_eq!(part_1(&parse(&test_input)), test_answer);
    }

    #[test]
    fn test_part_2() {
        let test_input = "16,1,2,0,4,2,7,1,2,14";
        let test_answer = 168;
        assert_eq!(part_2(&parse(&test_input)), test_answer);
    }
}
