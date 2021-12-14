use std::{collections::HashMap, fs, time::Instant};

type Input<'a> = (&'a str, HashMap<String, [String; 2]>);

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
    let mut parts = input.trim().split("\n\n");
    (
        parts.next().unwrap(),
        parts
            .next()
            .unwrap()
            .lines()
            .map(|l| {
                let mut ingredients = l.split(" -> ");
                let a = ingredients.next().unwrap();
                let b = ingredients.next().unwrap();
                (
                    a.to_string(),
                    [
                        format!("{}{}", a.get(0..1).unwrap(), b),
                        format!("{}{}", b, a.get(1..2).unwrap()),
                    ],
                )
            })
            .collect(),
    )
}

fn part_1(input: &Input) -> usize {
    get_ans(input, 10)
}

fn part_2(input: &Input) -> usize {
    get_ans(input, 40)
}

fn get_ans((start, rules): &Input, steps: usize) -> usize {
    let mut parts = HashMap::with_capacity(rules.len());
    for (a, b) in start.chars().zip(start.chars().skip(1)) {
        *parts.entry(String::from_iter([a, b])).or_insert(0) += 1;
    }
    for _ in 0..steps {
        let mut new_parts = HashMap::with_capacity(rules.len());

        for (part, count) in parts {
            if let Some([a, b]) = rules.get(&part) {
                *new_parts.entry(a.to_owned()).or_insert(0) += count;
                *new_parts.entry(b.to_owned()).or_insert(0) += count;
            } else {
                *new_parts.entry(part).or_insert(0) += count;
            }
        }
        parts = new_parts;
    }
    let mut counts = HashMap::with_capacity(parts.len() * 2);
    for (part, count) in parts {
        *counts.entry(part.chars().next().unwrap()).or_insert(0) += count;
    }
    *counts
        .entry(start.chars().next_back().unwrap())
        .or_insert(0) += 1;

    counts.values().max().unwrap() - counts.values().min().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "
NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 1588);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 2188189693529);
    }
}
