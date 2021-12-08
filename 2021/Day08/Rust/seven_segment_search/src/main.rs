use std::{collections::HashMap, fs, time::Instant};

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
    let mut ans = 0;
    for (a, b) in input {
        let mut mapping = HashMap::with_capacity(7);
        let sets: Vec<Vec<_>> = [a.to_owned(), b.to_owned()]
            .concat()
            .iter()
            .map(|d| d.chars().collect())
            .collect();
        let mut one = None;
        let mut four = None;
        for set in &sets {
            if set.len() == 2 {
                mapping.insert(set, 1);
                one = Some(set);
            } else if set.len() == 4 {
                mapping.insert(set, 4);
                four = Some(set);
            } else if set.len() == 3 {
                mapping.insert(set, 7);
            } else if set.len() == 7 {
                mapping.insert(set, 8);
            }
        }

        // 5s = 2, 3, 5
        // 6s = 0, 6, 9

        // 2: 1-1 4-2
        // 3: 1-2 4-3
        // 5: 1-1 4-3

        // 0: 1-2 4-3
        // 6: 1-1 4-2
        // 9: 1-2 4-4

        for set in &sets {
            let match_1 = count_same(set, one.unwrap());
            let match_4 = count_same(set, four.unwrap());
            if set.len() == 5 {
                match (match_1, match_4) {
                    (1, 2) => mapping.insert(set, 2),
                    (2, 3) => mapping.insert(set, 3),
                    (1, 3) => mapping.insert(set, 5),
                    _ => {
                        println!("5 {:?}", (match_1, match_4));
                        None
                    }
                };
            } else if set.len() == 6 {
                match (match_1, match_4) {
                    (2, 3) => mapping.insert(set, 0),
                    (1, 3) => mapping.insert(set, 6),
                    (2, 4) => mapping.insert(set, 9),
                    _ => {
                        println!("6 {:?}", (match_1, match_4));
                        None
                    }
                };
            }
        }

        for (i, d) in b.iter().rev().enumerate() {
            ans += usize::pow(10, i as u32) * mapping.get(&d.chars().collect::<Vec<_>>()).unwrap();
        }
    }
    ans
}

fn count_same<T>(a: &[T], b: &[T]) -> usize
where
    T: std::cmp::PartialEq,
{
    a.iter().filter(|x| b.contains(x)).count()
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
