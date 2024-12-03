type Input<'a> = Vec<Op>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Op {
    Do,
    Dont,
    Val(usize),
}

/// Parser for 2024 Day 03 (`mull_it_over`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let mut res = Vec::new();
    let mut i = 0;
    while i < input.trim().len() {
        if input[i..].starts_with("do()") {
            res.push(Op::Do);
            i += 4;
            continue;
        } else if input[i..].starts_with("don't()") {
            res.push(Op::Dont);
            i += 7;
            continue;
        } else if input[i..].starts_with("mul(") {
            let a_opt = input[i + 4..].split_once(',');

            i += 4;
            if a_opt.is_none() {
                continue;
            }
            let (a, tail) = a_opt.unwrap();
            let b_opt = tail.split_once(')');
            if b_opt.is_none() {
                continue;
            }
            let (b, _) = b_opt.unwrap();
            if let (Ok(a_val), Ok(b_val)) = (a.parse::<usize>(), b.parse::<usize>()) {
                res.push(Op::Val(a_val * b_val));
            }
            continue;
        }
        i += 1;
    }
    res
}

/// Solver for part 1 of 2024 Day 03 (`mull_it_over`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .map(|o| match o {
            Op::Dont | Op::Do => 0,
            Op::Val(x) => *x,
        })
        .sum()
}

/// Solver for part 2 of 2024 Day 03 (`mull_it_over`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &[Op]) -> usize {
    if input.is_empty() {
        0
    } else {
        input
            .iter()
            .take_while(|&op| op != &Op::Dont)
            .map(|o| match o {
                Op::Do | Op::Dont => 0,
                Op::Val(x) => *x,
            })
            .sum::<usize>()
            + part_2(
                &input
                    .iter()
                    .skip_while(|&op| op != &Op::Dont)
                    .skip_while(|&op| op != &Op::Do)
                    .copied()
                    .collect::<Vec<_>>(),
            )
    }
}
