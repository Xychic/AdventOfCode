use std::collections::{HashMap, HashSet, VecDeque};

type Input<'a> = HashMap<&'a str, HashSet<&'a str>>;

/// Parser for 2023 Day 25 (`snowverload`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut connections = HashMap::new();
    for line in input.trim().lines() {
        let (parent, children) = line.split_once(": ").unwrap();
        for child in children.split_ascii_whitespace() {
            connections
                .entry(parent)
                .or_insert(HashSet::new())
                .insert(child);
            connections
                .entry(child)
                .or_insert(HashSet::new())
                .insert(parent);
        }
    }
    connections
}

fn get_path<'a>(graph: &'a Input, start: &'a str, dest: &str) -> Option<Vec<&'a str>> {
    let mut queue = VecDeque::new();
    let mut seen = HashSet::with_capacity(graph.len());

    queue.push_back((start, vec![start]));

    while let Some((node, path)) = queue.pop_front() {
        if node == dest {
            return Some(path);
        }
        if seen.contains(node) {
            continue;
        }
        seen.insert(node);
        if let Some(children) = graph.get(node) {
            for &child in children {
                let mut new_path = path.clone();
                new_path.push(child);
                queue.push_back((child, new_path));
            }
        }
    }

    None
}

fn get_connected<'a>(graph: &'a Input, node: &'a str) -> HashSet<&'a str> {
    fn get_connected_inner<'a>(graph: &'a Input, node: &'a str, visited: &mut HashSet<&'a str>) {
        visited.insert(node);
        if let Some(children) = graph.get(node) {
            for &child in children {
                if visited.contains(&child) {
                    continue;
                }
                get_connected_inner(graph, child, visited);
            }
        }
    }

    let mut visited = HashSet::new();
    get_connected_inner(graph, node, &mut visited);
    visited
}

fn min_cut<'a>(graph: &'a Input, start: &'a str, dest: &'a str) -> (usize, (usize, usize)) {
    let mut cut_graph = graph.to_owned();

    while let Some(path) = get_path(&cut_graph, start, dest) {
        let mut new_graph = cut_graph.clone();

        for (&from, &to) in path.iter().zip(path.iter().skip(1)) {
            new_graph.get_mut(from).unwrap().remove(to);
            new_graph.get_mut(to).unwrap().remove(from);
        }
        cut_graph = new_graph;
    }
    let connected = get_connected(&cut_graph, start);
    let mut cuts = 0;
    for node in &connected {
        for dest in graph.get(node).unwrap() {
            if connected.contains(dest) {
                continue;
            }
            cuts += 1;
        }
    }
    (
        cuts,
        (connected.len(), get_connected(&cut_graph, dest).len()),
    )
}

/// Solver for part 1 of 2023 Day 25 (`snowverload`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    for start in input.keys() {
        for dest in input.keys() {
            if start == dest {
                continue;
            }
            let (cuts, (a_size, b_size)) = min_cut(input, start, dest);
            if cuts == 3 {
                return a_size * b_size;
            }
        }
    }
    unreachable!()
}
