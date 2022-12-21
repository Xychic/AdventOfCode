use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Monkey<'a> {
    Value(usize),
    Equation(&'a str, &'a str, &'a str),
}

impl<'a> Monkey<'a> {
    fn get_value(&self, others: &Input) -> usize {
        match self {
            Monkey::Value(x) => *x,
            Monkey::Equation(left, op, right) => {
                let left = others.get(&left.to_string()).unwrap();
                let right = others.get(&right.to_string()).unwrap();
                let left_val = left.get_value(others);
                let right_val = right.get_value(others);
                match *op {
                    "+" => left_val.saturating_add(right_val),
                    "-" => left_val.saturating_sub(right_val),
                    "*" => left_val.saturating_mul(right_val),
                    "/" => left_val.saturating_div(right_val),
                    _ => unreachable!(),
                }
            }
        }
    }
}

type Input<'a> = HashMap<String, Monkey<'a>>;

pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let (name, value) = l.split_once(' ').unwrap();
            let parts: Vec<_> = value.split(' ').collect();
            let name = name.strip_suffix(':').unwrap().to_owned();
            if parts.len() == 1 {
                (name, Monkey::Value(parts[0].parse().unwrap()))
            } else {
                (name, Monkey::Equation(parts[0], parts[1], parts[2]))
            }
        })
        .collect()
}

pub fn part_1(input: &Input) -> usize {
    let input = input.to_owned();
    let root = input.get(&"root".to_owned()).unwrap();
    root.get_value(&input)
}

pub fn part_2(input: &Input) -> usize {
    let mut input = input.to_owned();
    let (left, _, right) =
        if let Monkey::Equation(left, op, right) = input.get(&"root".to_owned()).unwrap().clone() {
            (left, op, right)
        } else {
            unreachable!()
        };
    let mut low = 0;
    let mut high = usize::MAX;
    let mut mid;
    loop {
        mid = (low + high) / 2;
        input.insert("humn".to_string(), Monkey::Value(mid));
        let x = input.get(&left.to_string()).unwrap().get_value(&input);
        let y = input.get(&right.to_string()).unwrap().get_value(&input);
        if x == y {
            break;
        } else if x < y {
            low = mid
        } else {
            high = mid
        }
    }

    mid as usize
}
