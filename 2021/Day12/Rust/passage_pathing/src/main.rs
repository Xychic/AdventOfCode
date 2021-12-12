use std::{collections::HashMap, fs, time::Instant};

type Input<'a> = HashMap<&'a str, Vec<&'a str>>;

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
    let connections: Vec<_> = input
        .trim()
        .lines()
        .map(|l| {
            let mut parts = l.split('-');
            (parts.next().unwrap(), parts.next().unwrap())
        })
        .collect();
    let mut paths = HashMap::with_capacity(input.len());
    for (k, v) in connections {
        paths
            .entry(k)
            .or_insert(Vec::with_capacity(input.len()))
            .push(v);
        paths
            .entry(v)
            .or_insert(Vec::with_capacity(input.len()))
            .push(k);
    }
    paths
}

fn part_1(input: &Input) -> usize {
    get_paths(input, "start", vec!["start"], &|path, cave| {
        !is_little(cave) || !path.contains(&cave)
    })
}

fn part_2(input: &Input) -> usize {
    get_paths(input, "start", vec!["start"], &|path, cave| {
        cave != "start" && (!is_little(cave) || !path.contains(&cave) || !max_little_visit(&path))
    })
}

fn get_paths(
    connections: &HashMap<&str, Vec<&str>>,
    current: &str,
    seen: Vec<&str>,
    valid: &dyn Fn(&Vec<&str>, &str) -> bool,
) -> usize {
    if current == "end" {
        return 1;
    }

    connections
        .get(&current)
        .unwrap()
        .iter()
        .map(|p| {
            if valid(&seen, p) {
                let mut new_path = Vec::with_capacity(seen.len() + 1);
                for x in &seen {
                    new_path.push(*x);
                }
                new_path.push(p);
                get_paths(connections, p, new_path, valid)
            } else {
                0
            }
        })
        .sum()
}

fn is_little(cave: &str) -> bool {
    cave.chars().all(|c| c.is_lowercase())
}

fn max_little_visit(path: &Vec<&str>) -> bool {
    path.iter()
        .filter(|c| is_little(c))
        .any(|c| path.iter().filter(|&a| a == c).count() >= 2)
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 19);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 103);
    }
}
