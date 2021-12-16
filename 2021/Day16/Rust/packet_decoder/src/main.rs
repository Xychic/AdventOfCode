use std::{collections::VecDeque, fs, time::Instant};

#[derive(Debug, Clone)]
struct Packet {
    version: usize,
    type_id: usize,
    data: Option<usize>,
    sub_packets: Option<Vec<Packet>>,
}

impl Packet {
    fn new(input: &mut VecDeque<char>) -> Packet {
        let version = usize::from_str_radix(&get_n(input, 3), 2).unwrap();
        let type_id = usize::from_str_radix(&get_n(input, 3), 2).unwrap();

        if type_id == 4 {
            let mut data = String::new();
            loop {
                let cont = input.pop_front().unwrap().to_digit(10).unwrap();
                data += &get_n(input, 4);
                if cont == 0 {
                    break;
                }
            }
            return Packet {
                version,
                type_id,
                data: Some(usize::from_str_radix(&data, 2).unwrap()),
                sub_packets: None,
            };
        } else {
            let length_type_id = input.pop_front().unwrap().to_digit(10).unwrap();
            let l;
            if length_type_id == 0 {
                l = usize::from_str_radix(&get_n(input, 15), 2).unwrap();
                let mut sub = Vec::new();
                let target_len = input.len() - l;
                while input.len() > target_len {
                    sub.push(Packet::new(input));
                }
                return Packet {
                    version,
                    type_id,
                    data: None,
                    sub_packets: Some(sub),
                };
            } else {
                l = usize::from_str_radix(&get_n(input, 11), 2).unwrap();
                return Packet {
                    version,
                    type_id,
                    data: None,
                    sub_packets: Some((0..l).map(|_| Packet::new(input)).collect()),
                };
            }
        }
    }

    fn sum_version(self) -> usize {
        let mut ans = self.version;
        if let Some(sub) = self.sub_packets {
            for p in sub {
                ans += p.sum_version();
            }
        }
        ans
    }

    fn eval(&self) -> usize {
        match self.type_id {
            0 => self
                .sub_packets
                .as_ref()
                .unwrap()
                .iter()
                .map(|p| p.eval())
                .sum(),
            1 => self
                .sub_packets
                .as_ref()
                .unwrap()
                .iter()
                .map(|p| p.eval())
                .product(),
            2 => self
                .sub_packets
                .as_ref()
                .unwrap()
                .iter()
                .map(|p| p.eval())
                .min()
                .unwrap(),
            3 => self
                .sub_packets
                .as_ref()
                .unwrap()
                .iter()
                .map(|p| p.eval())
                .max()
                .unwrap(),
            4 => self.data.unwrap(),
            5 => {
                let packets = self.sub_packets.as_ref().unwrap();
                if &packets[0].eval() > &packets[1].eval() {
                    1
                } else {
                    0
                }
            }
            6 => {
                let packets = self.sub_packets.as_ref().unwrap();
                if &packets[0].eval() < &packets[1].eval() {
                    1
                } else {
                    0
                }
            }
            7 => {
                let packets = self.sub_packets.as_ref().unwrap();
                if &packets[0].eval() == &packets[1].eval() {
                    1
                } else {
                    0
                }
            }
            _ => unreachable!(),
        }
    }
}

type Input = VecDeque<char>;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");

    let start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1(&raw_input),
        Instant::now() - start
    );
    let start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2(&raw_input),
        Instant::now() - start
    );
}

fn parse(input: &str) -> Input {
    let bin: String = input
        .trim()
        .chars()
        .map(|c| format!("{:0>4b}", c.to_digit(16).unwrap()))
        .collect();
    bin.chars().collect()
}

fn part_1(input: &str) -> usize {
    Packet::new(&mut parse(input)).sum_version()
}

fn part_2(input: &str) -> usize {
    Packet::new(&mut parse(input)).eval()
}

fn get_n(queue: &mut VecDeque<char>, n: usize) -> String {
    (0..n).map(|_| queue.pop_front().unwrap()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "8A004A801A8002F478";
    const TEST_INPUT_2: &str = "9C0141080250320F1802104A08";

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&TEST_INPUT_1), 16);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&TEST_INPUT_2), 1);
    }
}
