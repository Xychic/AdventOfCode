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
    let valid: Vec<_> = (0..=9).map(|d| digit_to_string(d).to_string()).collect();

    let mut ans = 0;
    for (a, b) in input {
        for p in "abcdefg".chars().permutations(7) {
            let mut swaps = HashMap::with_capacity(7);
            for (k, v) in p.iter().zip("abcdefg".chars()) {
                swaps.insert(k, v);
            }
            let valid = a.iter().all(|&s| {
                valid.contains(
                    &s.chars()
                        .sorted()
                        .map(|c| swaps.get(&c).unwrap())
                        .sorted()
                        .collect::<String>(),
                )
            });

            if valid {
                ans += b
                    .iter()
                    .rev()
                    .enumerate()
                    .map(|(i, &s)| {
                        usize::pow(10, i as u32)
                            * string_to_digit(
                                &s.chars()
                                    .sorted()
                                    .map(|c| swaps.get(&c).unwrap())
                                    .sorted()
                                    .collect::<String>(),
                            )
                    })
                    .sum::<usize>();
                continue;
            }
        }
    }
    ans
}

fn string_to_digit(input: &str) -> usize {
    match input {
        "abcefg" => 0,
        "cf" => 1,
        "acdeg" => 2,
        "acdfg" => 3,
        "bcdf" => 4,
        "abdfg" => 5,
        "abdefg" => 6,
        "acf" => 7,
        "abcdefg" => 8,
        "abcdfg" => 9,
        _ => unreachable!(),
    }
}

fn digit_to_string(input: usize) -> &'static str {
    match input {
        0 => "abcefg",
        1 => "cf",
        2 => "acdeg",
        3 => "acdfg",
        4 => "bcdf",
        5 => "abdfg",
        6 => "abdefg",
        7 => "acf",
        8 => "abcdefg",
        9 => "abcdfg",
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
