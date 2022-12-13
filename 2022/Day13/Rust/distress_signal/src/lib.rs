use std::{cmp::Ordering, fmt::Debug};

type Input = Vec<(Packet, Packet)>;

#[derive(PartialEq, Eq, Clone)]
pub enum Packet {
    Int(usize),
    List(Vec<Packet>),
}

impl Packet {
    fn parse(line: &str) -> Self {
        if line.starts_with('[') {
            if line.len() == 2 {
                Packet::List(Vec::new())
            } else {
                let to_parse = &line[1..line.len() - 1];
                let mut bracket_count = 0;
                let mut collector = String::with_capacity(to_parse.len());
                let mut sub_packets =
                    Vec::with_capacity(to_parse.chars().filter(|c| *c == ',').count());

                for chr in to_parse.chars() {
                    match chr {
                        '[' => {
                            bracket_count += 1;
                            collector += "[";
                        }
                        ']' => {
                            bracket_count -= 1;
                            collector += "]";
                        }
                        ',' => {
                            if bracket_count == 0 {
                                sub_packets.push(Packet::parse(&collector));
                                collector.clear();
                            } else {
                                collector += ",";
                            }
                        }
                        c => collector += &c.to_string(),
                    }
                }
                if !collector.is_empty() {
                    sub_packets.push(Packet::parse(&collector));
                }
                Packet::List(sub_packets)
            }
        } else {
            Packet::Int(line.parse().unwrap())
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Packet::Int(a), Packet::Int(b)) => a.cmp(b),
            (Packet::List(xs), Packet::List(ys)) => {
                for (x, y) in xs.iter().zip(ys.iter()) {
                    match x.cmp(y) {
                        Ordering::Equal => continue,
                        res => return res,
                    }
                }
                xs.len().cmp(&ys.len())
            }
            (Packet::Int(a), p @ Packet::List(_)) => Packet::List(vec![Packet::Int(*a)]).cmp(p),
            (p @ Packet::List(_), Packet::Int(b)) => p.cmp(&Packet::List(vec![Packet::Int(*b)])),
        }
    }
}

impl Debug for Packet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(x) => write!(f, "{x}"),
            Self::List(xs) => {
                write!(
                    f,
                    "[{}]",
                    xs.iter()
                        .map(|x| format!("{x:?}"))
                        .collect::<Vec<_>>()
                        .join(",")
                )
            }
        }
    }
}

pub fn parse(input: &str) -> Input {
    input
        .trim()
        .split("\n\n")
        .map(|group| {
            let (a, b) = group.split_once('\n').unwrap();
            (Packet::parse(a), Packet::parse(b))
        })
        .collect()
}

pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .enumerate()
        .filter(|(_, (a, b))| a < b)
        .map(|(x, _)| x + 1)
        .sum()
}

pub fn part_2(input: &Input) -> usize {
    let mut packets = Vec::with_capacity(2 + 2 * input.len());

    packets.push(Packet::parse("[[2]]"));
    packets.push(Packet::parse("[[6]]"));

    for (a, b) in input {
        packets.push(a.clone());
        packets.push(b.clone());
    }

    packets.sort();

    let mut ans = 1;
    for (i, p) in packets.iter().enumerate() {
        match format!("{p:?}").as_str() {
            "[[2]]" => ans *= i + 1,
            "[[6]]" => {
                ans *= i + 1;
                break;
            }
            _ => continue,
        }
    }

    ans
}
