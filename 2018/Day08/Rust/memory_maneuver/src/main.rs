use std::{collections::VecDeque, fs};

type Input = VecDeque<usize>;

#[derive(Debug)]
struct Node {
    children: Vec<Node>,
    metadata: Vec<usize>,
}

impl Node {
    fn sum_meta(&self) -> usize {
        let mut sum = 0;
        sum += self.metadata.iter().sum::<usize>();
        sum += self.children.iter().map(|c| c.sum_meta()).sum::<usize>();
        sum
    }

    fn val(&self) -> usize {
        if self.children.is_empty() {
            return self.metadata.iter().sum::<usize>();
        }
        self.metadata
            .iter()
            .filter(|&i| i <= &self.children.len())
            .map(|i| self.children[i - 1].val())
            .sum::<usize>()
    }
}

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(input.trim());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn parse(input: &str) -> Input {
    input.split(' ').map(|x| x.parse().unwrap()).collect()
}

fn part_1(input: &Input) -> usize {
    make_tree(&mut input.to_owned()).sum_meta()
}

fn part_2(input: &Input) -> usize {
    make_tree(&mut input.to_owned()).val()
}

fn make_tree(numbers: &mut VecDeque<usize>) -> Node {
    let mut children = Vec::new();
    let mut metadata = Vec::new();
    let child_count = numbers.pop_front().unwrap();
    let metadata_count = numbers.pop_front().unwrap();
    for _ in 0..child_count {
        children.push(make_tree(numbers));
    }
    for _ in 0..metadata_count {
        metadata.push(numbers.pop_front().unwrap());
    }

    Node { children, metadata }
}
