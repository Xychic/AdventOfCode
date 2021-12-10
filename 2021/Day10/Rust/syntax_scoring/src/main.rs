use std::{collections::VecDeque, fs, time::Instant};

type Input<'a> = Vec<&'a str>;

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
    input.lines().collect()
}

fn part_1(input: &Input) -> usize {
    let mut score = 0;
    for &line in input {
        let mut stack = VecDeque::with_capacity(line.len());
        for c in line.chars() {
            match c {
                ')' | ']' | '}' | '>' => {
                    if stack.pop_back().unwrap() != c {
                        score += match c {
                            ')' => 3,
                            ']' => 57,
                            '}' => 1197,
                            '>' => 25137,
                            _ => unreachable!(),
                        };
                        break;
                    }
                }
                '(' | '[' | '{' | '<' => stack.push_back(match c {
                    '(' => ')',
                    '{' => '}',
                    '[' => ']',
                    '<' => '>',
                    _ => unreachable!(),
                }),
                _ => unreachable!(),
            }
        }
    }
    score
}

fn part_2(input: &Input) -> usize {
    let mut scores = Vec::with_capacity(input.len());
    'lines: for &line in input {
        let mut stack = VecDeque::with_capacity(line.len());
        for c in line.chars() {
            match c {
                ')' | ']' | '}' | '>' => {
                    if stack.pop_back().unwrap() != c {
                        continue 'lines;
                    }
                }
                '(' | '[' | '{' | '<' => stack.push_back(match c {
                    '(' => ')',
                    '{' => '}',
                    '[' => ']',
                    '<' => '>',
                    _ => unreachable!(),
                }),
                _ => unreachable!(),
            }
        }
        scores.push(
            stack
                .iter()
                .rev()
                .map(|c| match c {
                    ')' => 1,
                    ']' => 2,
                    '}' => 3,
                    '>' => 4,
                    _ => unreachable!(),
                })
                .fold(0, |acc, x| 5 * acc + x),
        );
    }
    scores.sort_unstable();
    scores[(scores.len() - 1) / 2]
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 26397);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 288957);
    }
}
