use regex::Regex;
use std::fs;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum OpCode {
    Addr,
    Addi,
    Mulr,
    Muli,
    Banr,
    Bani,
    Borr,
    Bori,
    Setr,
    Seti,
    Gtir,
    Gtri,
    Gtrr,
    Eqir,
    Eqri,
    Eqrr,
}

impl OpCode {
    fn apply(self, a: usize, b: usize, c: usize, data: &mut Vec<usize>) {
        match self {
            OpCode::Addr => (data[c] = data[a] + data[b]),
            OpCode::Addi => (data[c] = data[a] + b),
            OpCode::Mulr => (data[c] = data[a] * data[b]),
            OpCode::Muli => (data[c] = data[a] * b),
            OpCode::Banr => (data[c] = data[a] & data[b]),
            OpCode::Bani => (data[c] = data[a] & b),
            OpCode::Borr => (data[c] = data[a] | data[b]),
            OpCode::Bori => (data[c] = data[a] | b),
            OpCode::Setr => (data[c] = data[a]),
            OpCode::Seti => (data[c] = a),
            OpCode::Gtir => (data[c] = if a > data[b] { 1 } else { 0 }),
            OpCode::Gtri => (data[c] = if data[a] > b { 1 } else { 0 }),
            OpCode::Gtrr => (data[c] = if data[a] > data[b] { 1 } else { 0 }),
            OpCode::Eqir => (data[c] = if a == data[b] { 1 } else { 0 }),
            OpCode::Eqri => (data[c] = if data[a] == b { 1 } else { 0 }),
            OpCode::Eqrr => (data[c] = if data[a] == data[b] { 1 } else { 0 }),
        }
    }
}

type Input = (usize, Vec<(OpCode, usize, usize, usize)>);

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn parse(input: &str) -> Input {
    let rows: Vec<_> = input.trim().split('\n').collect();
    let ip = rows[0].split(' ').skip(1).next().unwrap().parse().unwrap();
    let re = Regex::new(r"([a-z]{4}) ([0-9]+) ([0-9]+) ([0-9]+)").unwrap();
    let data: Vec<_> = rows
        .iter()
        .skip(1)
        .map(|&row| {
            let caps = re.captures(row).unwrap();
            let action = match &caps[1] {
                "addr" => OpCode::Addr,
                "addi" => OpCode::Addi,
                "mulr" => OpCode::Mulr,
                "muli" => OpCode::Muli,
                "banr" => OpCode::Banr,
                "bani" => OpCode::Bani,
                "borr" => OpCode::Borr,
                "bori" => OpCode::Bori,
                "setr" => OpCode::Setr,
                "seti" => OpCode::Seti,
                "gtir" => OpCode::Gtir,
                "gtri" => OpCode::Gtri,
                "gtrr" => OpCode::Gtrr,
                "eqir" => OpCode::Eqir,
                "eqri" => OpCode::Eqri,
                "eqrr" => OpCode::Eqrr,
                _ => (panic!("Unknown type {}", &caps[1])),
            };
            let a: usize = caps[2].parse().unwrap();
            let b: usize = caps[3].parse().unwrap();
            let c: usize = caps[4].parse().unwrap();
            (action, a, b, c)
        })
        .collect();
    (ip, data)
}

fn part_1((ip, data): &Input) -> usize {
    let mut registers: Vec<usize> = vec![0; 6];
    let ip = *ip;

    while registers[ip] < data.len() {
        let (op, a, b, c) = data[registers[ip]];
        op.apply(a, b, c, &mut registers);
        registers[ip] += 1;
    }
    registers[0]
}

fn part_2((ip, data): &Input) -> usize {
    let mut registers: Vec<usize> = vec![0; 6];
    registers[0] = 1;
    let ip = *ip;

    while registers[ip] < data.len() {
        let (op, a, b, c) = data[registers[ip]];
        op.apply(a, b, c, &mut registers);
        registers[ip] += 1;

        if registers[0] == 0 {
            break;
        }
    }

    let num = registers[4];
    (0..=num)
        .into_iter()
        .filter(|&x| (num as f64 / x as f64).fract() == 0.0)
        .sum()
}
