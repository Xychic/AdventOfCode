type Input<'a> = str;

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Mul,
}

/// Solver for part 1 of 2025 Day 06 (`trash_compactor`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    let lines: Vec<_> = input.trim().lines().collect();

    let mut parsed: Vec<(Op, Vec<usize>)> = lines
        .last()
        .unwrap()
        .split_whitespace()
        .map(|x| match x {
            "*" => (Op::Mul, Vec::new()),
            "+" => (Op::Add, Vec::new()),
            _ => unreachable!(),
        })
        .collect();
    for &l in &lines[..lines.len() - 1] {
        for (i, s) in l.split_whitespace().enumerate() {
            let (_, arr) = parsed.get_mut(i).unwrap();
            arr.push(s.parse().unwrap());
        }
    }
    parsed
        .iter()
        .map(|(op, xs)| match op {
            Op::Mul => xs.iter().product::<usize>(),
            Op::Add => xs.iter().sum(),
        })
        .sum()
}

/// Solver for part 2 of 2025 Day 06 (`trash_compactor`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    let lines: Vec<_> = input.lines().collect();

    let mut parsed: Vec<(Op, Vec<usize>)> = lines
        .last()
        .unwrap()
        .split_whitespace()
        .map(|x| match x {
            "*" => (Op::Mul, Vec::new()),
            "+" => (Op::Add, Vec::new()),
            _ => unreachable!(),
        })
        .collect();

    let len = lines[0].len();
    let mut iters: Vec<_> = lines
        .iter()
        .take(lines.len() - 1)
        .map(|l| l.chars())
        .collect();

    let mut index = 0;
    for val in (0..len).map(|_| {
        iters
            .iter_mut()
            .map(|n| n.next().unwrap())
            .filter(|&c| c != ' ')
            .fold(0, |acc, c| acc * 10 + c.to_digit(10).unwrap() as usize)
    }) {
        if val == 0 {
            index += 1;
            continue;
        }
        let (_, arr) = parsed.get_mut(index).unwrap();
        arr.push(val);
    }

    parsed
        .iter()
        .map(|(op, xs)| match op {
            Op::Mul => xs.iter().product::<usize>(),
            Op::Add => xs.iter().sum(),
        })
        .sum()
}
