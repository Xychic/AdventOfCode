use std::{
    collections::{HashMap, HashSet},
    fs,
    time::Instant,
};

use priority_queue::PriorityQueue;

type Input<'a> = (&'a str, HashMap<&'a str, &'a str>);

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
    let (replacements_string, molecule) = input.trim().split_once("\n\n").unwrap();
    let mut replacements = HashMap::new();
    for line in replacements_string.split('\n') {
        let (before, after) = line.split_once(" => ").unwrap();
        replacements.insert(after, before);
    }
    (molecule, replacements)
}

fn part_1((molecule, replacements): &Input) -> usize {
    let mut new_molecules = HashSet::new();
    for (k, v) in replacements {
        for (index, _) in molecule.match_indices(v) {
            let before = &molecule[..index];
            let after = &molecule[index + v.len()..];

            assert_eq!(molecule, &format!("{before}{v}{after}"));
            new_molecules.insert(format!("{before}{k}{after}"));
        }
    }
    new_molecules.len()
}

fn part_2((molecule, replacements): &Input) -> usize {
    let symbol_count = molecule.chars().filter(|c| c.is_uppercase()).count();
    let rn_count = molecule.matches("Rn").count();
    let ar_count = molecule.matches("Ar").count();
    let y_count = molecule.matches("Y").count();

    symbol_count - rn_count - ar_count - (2 * y_count) - 1

    // let mut pqueue = PriorityQueue::new();
    // let mut seen = HashSet::new();
    // pqueue.push((molecule.to_string(), 0), 0);
    // seen.insert(molecule.to_string());

    // let mut sorted_replacements: Vec<_> = replacements.iter().collect();
    // sorted_replacements.sort_by_key(|(a, b)| usize::MAX - a.len());
    // // dbg!(sorted_replacements);
    // while let Some(((mol, steps), _)) = pqueue.pop() {
    //     if mol == "e" {
    //         return steps;
    //     }
    //     println!("{steps}: {}", mol.len());
    //     // dbg!(&pqueue);
    //     for (k, v) in &sorted_replacements {
    //         // println!("\t{k}: {v}");
    //         for (index, _) in mol.match_indices(*k) {
    //             let before = &mol[..index];
    //             let after = &mol[index + k.len()..];

    //             // assert_eq!(mol, format!("{before}{k}{after}"));

    //             let new = format!("{before}{v}{after}");

    //             if seen.contains(&new) {
    //                 continue;
    //             }
    //             pqueue.push((new.clone(), steps + 1), steps + 1 + (1000 - new.len()));
    //             seen.insert(new);
    //         }
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    // #[test]
    // fn test_part_1() {
    //     assert_eq!(part_1(&parse(&TEST_INPUT_1)), 0);
    // }

    // #[test]
    // fn test_part_2() {
    //     assert_eq!(part_2(&parse(&TEST_INPUT_2)), 0);
    // }
}
