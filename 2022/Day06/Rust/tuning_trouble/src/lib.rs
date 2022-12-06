use itertools::Itertools;

type Input<'a> = Vec<char>;

pub fn parse(input: &str) -> Input {
    input.trim().chars().collect()
}

pub fn part_1(input: &Input) -> usize {
    find_after_n_distinct(input, 4)
}

pub fn part_2(input: &Input) -> usize {
    find_after_n_distinct(input, 14)
}

fn find_after_n_distinct(input: &Input, distinct: usize) -> usize {
    input
        .windows(distinct)
        .enumerate()
        .filter(|(_, x)| x.iter().unique().count() == distinct)
        .next()
        .unwrap()
        .0
        + distinct
}
