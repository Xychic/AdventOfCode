use std::collections::{HashMap, VecDeque};

type Rules<'a> = HashMap<&'a str, Vec<Rule<'a>>>;
type PartRange = [(usize, usize); 4];
type Input<'a> = (Rules<'a>, Vec<Part>);

#[derive(Debug, Clone, Copy)]
pub struct Part {
    pub x: usize,
    pub m: usize,
    pub a: usize,
    pub s: usize,
}

impl Part {
    fn accepted(&self, rules: &Rules) -> bool {
        let mut state = "in";
        loop {
            if state == "A" {
                return true;
            } else if state == "R" {
                return false;
            }
            let to_check = rules.get(state).unwrap();
            for rule in to_check {
                state = match rule.check(self) {
                    Some(s) => s,
                    None => continue,
                };
                break;
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Rule<'a> {
    Default(&'a str),
    Check(&'a str, &'a str, usize, &'a str),
}

impl<'a> Rule<'a> {
    fn check(&self, part: &Part) -> Option<&'a str> {
        match self {
            Rule::Default(dest) => Some(dest),
            Rule::Check(key, op, val, dest) => {
                let part_val = match *key {
                    "x" => part.x,
                    "m" => part.m,
                    "a" => part.a,
                    "s" => part.s,
                    _ => unreachable!(),
                };
                let valid = match *op {
                    "<" => part_val < *val,
                    ">" => part_val > *val,
                    _ => unreachable!(),
                };
                // dbg!(key, part_val, op, val, dest, valid);
                if valid {
                    Some(dest)
                } else {
                    None
                }
            }
        }
    }

    fn check_range(&self, vals: PartRange) -> (Option<(&str, PartRange)>, Option<PartRange>) {
        match self {
            Rule::Default(dest) => (Some((dest, vals)), None),
            Rule::Check(key, op, val, dest) => {
                let [(mut x_low, mut x_high), (mut m_low, mut m_high), (mut a_low, mut a_high), (mut s_low, mut s_high)] =
                    vals;
                let [(mut x_low_rem, mut x_high_rem), (mut m_low_rem, mut m_high_rem), (mut a_low_rem, mut a_high_2), (mut s_low_rem, mut s_high_rem)] =
                    vals;

                match *key {
                    "x" => {
                        ((x_low_rem, x_high_rem), (x_low, x_high)) =
                            Rule::get_range(op, *val, (x_low, x_high));
                    }
                    "m" => {
                        ((m_low_rem, m_high_rem), (m_low, m_high)) =
                            Rule::get_range(op, *val, (m_low, m_high));
                    }
                    "a" => {
                        ((a_low_rem, a_high_2), (a_low, a_high)) =
                            Rule::get_range(op, *val, (a_low, a_high));
                    }
                    "s" => {
                        ((s_low_rem, s_high_rem), (s_low, s_high)) =
                            Rule::get_range(op, *val, (s_low, s_high));
                    }
                    _ => unreachable!(),
                };

                (
                    Some((
                        *dest,
                        [
                            (x_low_rem, x_high_rem),
                            (m_low_rem, m_high_rem),
                            (a_low_rem, a_high_2),
                            (s_low_rem, s_high_rem),
                        ],
                    )),
                    Some([
                        (x_low, x_high),
                        (m_low, m_high),
                        (a_low, a_high),
                        (s_low, s_high),
                    ]),
                )
            }
        }
    }

    fn get_range(
        op: &str,
        val: usize,
        (low, high): (usize, usize),
    ) -> ((usize, usize), (usize, usize)) {
        match op {
            "<" => ((low, high.min(val - 1)), (low.max(val), high)),
            ">" => ((low.max(val + 1), high), (low, high.min(val))),
            _ => unreachable!(),
        }
    }
}

/// Parser for 2023 Day 19 (`aplenty`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let (rules, parts) = input.split_once("\n\n").unwrap();
    (
        rules
            .lines()
            .map(|l| {
                let (state, rules) = l.split_once('{').unwrap();
                (
                    state,
                    rules
                        .split(',')
                        .map(|r| {
                            if r.contains(':') {
                                let (r, dest) = r.split_once(':').unwrap();
                                Rule::Check(&r[..1], &r[1..2], r[2..].parse().unwrap(), dest)
                            } else {
                                Rule::Default(&r[..r.len() - 1])
                            }
                        })
                        .collect(),
                )
            })
            .collect(),
        parts
            .lines()
            .map(|p| {
                let mut vals = p[..p.len() - 1]
                    .split(',')
                    .map(|v| v.split_once('=').unwrap().1.parse().unwrap());
                Part {
                    x: vals.next().unwrap(),
                    m: vals.next().unwrap(),
                    a: vals.next().unwrap(),
                    s: vals.next().unwrap(),
                }
            })
            .collect(),
    )
}

/// Solver for part 1 of 2023 Day 19 (`aplenty`)
///
/// # Panics
#[must_use]
pub fn part_1((rules, parts): &Input) -> usize {
    parts
        .iter()
        .filter(|p| p.accepted(rules))
        .map(|p| p.x + p.m + p.a + p.s)
        .sum()
}

/// Solver for part 2 of 2023 Day 19 (`aplenty`)
///
/// # Panics
#[must_use]
pub fn part_2((rules, _): &Input) -> usize {
    let mut queue = VecDeque::new();
    let mut ans = 0;
    queue.push_back(("in", [(1, 4000), (1, 4000), (1, 4000), (1, 4000)]));
    while let Some((
        state,
        [(mut x_low, mut x_high), (mut m_low, mut m_high), (mut a_low, mut a_high), (mut s_low, mut s_high)],
    )) = queue.pop_front()
    {
        if x_high < x_low || m_high < m_low || a_high < a_low || s_high < s_low {
            continue;
        }
        match state {
            "A" => {
                ans += (x_high - x_low + 1)
                    * (m_high - m_low + 1)
                    * (a_high - a_low + 1)
                    * (s_high - s_low + 1);
            }
            "R" => continue,
            _ => {
                let to_check = &rules[state];
                for rule in to_check {
                    // rule.check_range(&mut queue, vals);
                    match rule.check_range([
                        (x_low, x_high),
                        (m_low, m_high),
                        (a_low, a_high),
                        (s_low, s_high),
                    ]) {
                        (Some(v), Some(vals)) => {
                            queue.push_back(v);
                            [
                                (x_low, x_high),
                                (m_low, m_high),
                                (a_low, a_high),
                                (s_low, s_high),
                            ] = vals;
                        }
                        (Some(v), None) => queue.push_back(v),
                        (None, Some(vals)) => {
                            [
                                (x_low, x_high),
                                (m_low, m_high),
                                (a_low, a_high),
                                (s_low, s_high),
                            ] = vals;
                        }
                        (None, None) => unreachable!(),
                    }
                }
            }
        }
    }
    ans
}
