use std::{fs, ops::Add, panic, time::Instant};

#[derive(Debug)]
enum Number {
    Value(usize),
    Pair { a: Box<Number>, b: Box<Number> },
}

impl Number {
    fn new(line: &str) -> Number {
        if line.starts_with('[') {
            let (a, b) = split_bracket_block(&line[1..line.len() - 1]);
            Number::Pair {
                a: Box::new(Number::new(a)),
                b: Box::new(Number::new(b)),
            }
        } else {
            Number::Value(line.parse().unwrap())
        }
    }

    fn value(&self) -> usize {
        match self {
            Number::Value(a) => *a,
            Number::Pair { a, b } => panic!(),
        }
    }

    fn split(&mut self) {
        match self {
            Number::Value(x) => {
                let a = *x / 2;
                let b = (*x + 1) / 2;
                *self = Number::Pair {
                    a: Box::new(Number::Value(a)),
                    b: Box::new(Number::Value(b)),
                };
            }
            Number::Pair { a: _, b: _ } => panic!("Tried to split a pair"),
        }
    }

    fn explode(&mut self, depth: usize) -> (Option<usize>, Option<usize>) {
        println!("Depth {}: {:?}", depth, self);
        if depth == 4 {
            let vals = match self {
                Number::Value(_) => (None, None),
                Number::Pair { a, b } => (Some(a.value()), Some(b.value())),
            };
            *self = Number::Value(0);
            vals
        } else {
            // if let Some((left, right)) = match self {
            //     Number::Value(_) => None,
            //     Number::Pair { a, b } => {
            //         None
            //     },
            // } {
            //     println!("{:?}", left);
            //     println!("{:?}", right);
            // }
            (None, None)
        }
    }

    fn magnitude(self) -> usize {
        match self {
            Number::Value(x) => x,
            Number::Pair { a, b } => 3 * a.magnitude() + 2 * b.magnitude(),
        }
    }
}

impl Add for Number {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Number::Pair {
            a: Box::new(self),
            b: Box::new(other),
        }
    }
}

type Input = Vec<Number>;

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
    input.trim().lines().map(|l| Number::new(l)).collect()
}

fn part_1(input: &Input) -> usize {
    // dbg!(input);
    let a = Number::new("[1,2]");
    let b = Number::new("[[3,4],5]");
    let mut c = Number::new("[[[[[9,8],1],2],3],4]");
    c.explode(0);
    todo!()
}

fn part_2(input: &Input) -> usize {
    todo!()
}

fn split_bracket_block(block: &str) -> (&str, &str) {
    let mut bracket_count = 0;
    for (i, c) in block.char_indices() {
        if c == '[' {
            bracket_count += 1;
        } else if c == ']' {
            bracket_count -= 1;
        }
        if bracket_count == 0 {
            return (&block[0..i + 1], &block[i + 2..]);
        }
    }
    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 4140);
    }

    // #[test]
    // fn test_part_2() {
    //     assert_eq!(part_2(&parse(&TEST_INPUT_2)), 0);
    // }
}
