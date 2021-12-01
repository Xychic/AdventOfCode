use std::{
    collections::{HashMap, HashSet, VecDeque},
    fs,
};

type Input = HashMap<(isize, isize), HashSet<(isize, isize)>>;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn parse(input: &str) -> Input {
    let mut path = VecDeque::new();
    let mut graph: Input = HashMap::new();
    path.push_back((0, 0));

    for c in input.trim().chars() {
        if vec!['^', '$'].contains(&c) {
            continue;
        }
        let (x, y) = path.pop_back().unwrap();
        let mut w = None;
        match c {
            'N' => (w = Some((x, y + 1))),
            'E' => (w = Some((x + 1, y))),
            'S' => (w = Some((x, y - 1))),
            'W' => (w = Some((x - 1, y))),
            '(' => {
                path.push_back((x, y));
                path.push_back((x, y));
            }
            '|' => (path.push_back(*path.back().unwrap())),
            _ => (),
        }
        if let Some(w) = w {
            graph.entry((x, y)).or_insert_with(HashSet::new).insert(w);
            path.push_back(w);
        }
    }
    graph
}

fn part_1(input: &Input) -> usize {
    *get_dists(input, (0, 0))
        .iter()
        .max_by_key(|(_, &d)| d)
        .unwrap()
        .1
}

fn part_2(input: &Input) -> usize {
    get_dists(input, (0, 0))
        .iter()
        .filter(|(_, &d)| d >= 1000)
        .count()
}


fn get_dists(map: &Input, start: (isize, isize)) -> HashMap<(isize, isize), usize> {
    let mut queue = VecDeque::new();
    let mut seen = HashSet::new();
    let mut dists = HashMap::new();
    queue.push_back(start);
    dists.insert(start, 0);

    while let Some((x, y)) = queue.pop_front() {
        seen.insert((x, y));
        let d = *dists.get(&(x, y)).unwrap();
        for p in map.get(&(x, y)).unwrap_or(&HashSet::new()) {
            if !seen.contains(p) {
                queue.push_back(*p);
                dists.insert(*p, d + 1);
            }
        }
    }

    dists
}
