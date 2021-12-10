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
                ')' => {
                    if stack.back().unwrap() != &'(' {
                        score += 3;
                        break;
                    }
                    stack.pop_back();
                }
                ']' => {
                    if stack.back().unwrap() != &'[' {
                        score += 57;
                        break;
                    }
                    stack.pop_back();
                }
                '}' => {
                    if stack.back().unwrap() != &'{' {
                        score += 1197;
                        break;
                    }
                    stack.pop_back();
                }
                '>' => {
                    if stack.back().unwrap() != &'<' {
                        score += 25137;
                        break;
                    }
                    stack.pop_back();
                }
                _ => stack.push_back(c),
            }
        }
    }
    score
}

fn part_2(input: &Input) -> usize {
    let mut scores = Vec::with_capacity(input.len());
    for &line in input {
        let mut stack = VecDeque::with_capacity(line.len());
        let mut valid = true;
        for c in line.chars() {
            match c {
                ')' => {
                    if stack.back().unwrap() != &'(' {
                        valid = false;
                        break;
                    }
                    stack.pop_back();
                }
                ']' => {
                    if stack.back().unwrap() != &'[' {
                        valid = false;
                        break;
                    }
                    stack.pop_back();
                }
                '}' => {
                    if stack.back().unwrap() != &'{' {
                        valid = false;
                        break;
                    }
                    stack.pop_back();
                }
                '>' => {
                    if stack.back().unwrap() != &'<' {
                        valid = false;
                        break;
                    }
                    stack.pop_back();
                }
                _ => stack.push_back(c),
            }
        }
        if valid {
            scores.push(
                stack
                    .iter()
                    .rev()
                    .map(|c| get_closer_score(&c))
                    .fold(0, |acc, x| acc * 5 + x),
            );
        }
    }
    scores.sort();
    scores[(scores.len() - 1) / 2]
}

fn get_closer_score(c: &char) -> usize {
    match c {
        '(' => 1,
        '[' => 2,
        '{' => 3,
        '<' => 4,
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        let test_input = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]";
        let test_answer = 26397;
        assert_eq!(part_1(&parse(&test_input)), test_answer);
    }

    #[test]
    fn test_part_2() {
        let test_input = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]";
        let test_answer = 288957;
        assert_eq!(part_2(&parse(&test_input)), test_answer);
    }
}
