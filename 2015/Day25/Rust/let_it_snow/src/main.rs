use std::{fs, time::Instant};

use inpt::{inpt, Inpt};

#[derive(Debug, Inpt)]
#[inpt(
    regex = r"To continue, please consult the code grid in the manual.  Enter the code at row (\d+), column (\d+)."
)]
struct Input(usize, usize);

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    let start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1(&input),
        Instant::now() - start
    );
}

fn parse(input: &str) -> Input {
    inpt::<Input>(input.trim()).unwrap()
}

fn part_1(input: &Input) -> usize {
    let Input(y, x) = input;
    let mut val = 20151125;
    for index in 2.. {
        for i in 1..index {
            if *x == i && *y == (index - i) {
                return val;
            }
            val *= 252533;
            val %= 33554393;
        }
    }
    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "To continue, please consult the code grid in the manual.  Enter the code at row 6, column 5.";

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 1534922);
    }
}
