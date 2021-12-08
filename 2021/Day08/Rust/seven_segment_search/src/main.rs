use std::{collections::HashMap, fs, time::Instant};

use itertools::Itertools;

type Input<'a> = Vec<(Vec<&'a str>, Vec<&'a str>)>;

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
        .map(|line| {
            let mut parts = line.split(" | ");
            (
                parts.next().unwrap().split(' ').collect(),
                parts.next().unwrap().split(' ').collect(),
            )
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    input
        .iter()
        .map(|(_, b)| b.iter().filter(|x| [2, 4, 3, 7].contains(&x.len())).count())
        .sum()
}

fn part_2(input: &Input) -> usize {
    let valid: Vec<_> = (0..=9).map(|d| digit_to_val(d)).collect();

    let mut ans = 0;
    for (a, b) in input {
        for p in "abcdefg".chars().permutations(7) {
            let mut swaps = HashMap::with_capacity(7);
            for (k, v) in p.iter().zip(vec![1, 2, 4, 8, 16, 32, 64]) {
                swaps.insert(k, v);
            }
            let valid = a.iter().all(|&s| {
                valid.contains(&s.chars().map(|c| swaps.get(&c).unwrap()).sum::<usize>())
            });

            if valid {
                ans += b
                    .iter()
                    .rev()
                    .enumerate()
                    .map(|(i, &s)| {
                        usize::pow(10, i as u32)
                            * val_to_digit(s.chars().map(|c| swaps.get(&c).unwrap()).sum::<usize>())
                    })
                    .sum::<usize>();
                continue;
            }
        }
    }
    ans
}

fn val_to_digit(input: usize) -> usize {
    match input {
        119 => 0,
        36 => 1,
        93 => 2,
        109 => 3,
        46 => 4,
        107 => 5,
        123 => 6,
        37 => 7,
        127 => 8,
        111 => 9,
        _ => unreachable!(),
    }
}

fn digit_to_val(input: usize) -> usize {
    match input {
        0 => 119,
        1 => 36,
        2 => 93,
        3 => 109,
        4 => 46,
        5 => 107,
        6 => 123,
        7 => 37,
        8 => 127,
        9 => 111,
        _ => unreachable!(),
    }
}

fn char_to_val(input: &char) -> usize {
    match input {
        'a' => 1,
        'b' => 2,
        'c' => 4,
        'd' => 8,
        'e' => 16,
        'f' => 32,
        'g' => 64,
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        let test_input = "
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce";
        let test_answer = 26;
        assert_eq!(part_1(&parse(&test_input)), test_answer);
    }

    #[test]
    fn test_part_2() {
        let test_input = "
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce";
        let test_answer = 61229;
        assert_eq!(part_2(&parse(&test_input)), test_answer);
    }
}
