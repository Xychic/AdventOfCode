use std::{
    collections::{HashMap, VecDeque},
    fs,
    time::Instant,
};

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
    fn get_paths<'a>(connections: &'a HashMap<&str, Vec<&str>>, path: VecDeque<&'a str>) -> usize {
        let last = *path.back().unwrap();
        if last == "end" {
            return 1;
        }
        let possible = connections.get(&last).unwrap();
        let mut paths = 0;
        for &p in possible {
            if is_little(p) && path.contains(&p) {
                continue;
            }
            let mut new_path = path.clone();
            new_path.push_back(p);
            paths += get_paths(connections, new_path);
        }
        paths
    }
    get_paths(input, VecDeque::from(vec!["start"]))
}

fn part_2(input: &Input) -> usize {
    fn get_paths<'a>(connections: &'_ HashMap<&str, Vec<&str>>, path: VecDeque<&'a str>) -> usize {
        let last = *path.back().unwrap();
        if last == "end" {
            return 1;
        }
        let possible = connections.get(&last).unwrap();
        let mut paths = 0;
        for &p in possible {
            if p == "start" || (is_little(p) && path.contains(&p) && max_little_visit(&path) == 2) {
                continue;
            }
            let mut new_path = path.clone();
            new_path.push_back(p);
            paths += get_paths(connections, new_path);
        }
        paths
    }
    get_paths(input, VecDeque::from(vec!["start"]))
}

fn is_little(cave: &str) -> bool {
    cave.chars().all(|c| c.is_lowercase())
}

fn max_little_visit(path: &VecDeque<&str>) -> usize {
    path.iter()
        .filter(|c| is_little(c))
        .map(|c| path.iter().filter(|&a| a == c).count())
        .max()
        .unwrap()
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
