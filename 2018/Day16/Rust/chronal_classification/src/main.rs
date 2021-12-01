use std::{
    collections::{HashMap, HashSet},
    fs,
};

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

    fn iter() -> std::slice::Iter<'static, OpCode> {
        [
            OpCode::Addr,
            OpCode::Addi,
            OpCode::Mulr,
            OpCode::Muli,
            OpCode::Banr,
            OpCode::Bani,
            OpCode::Borr,
            OpCode::Bori,
            OpCode::Setr,
            OpCode::Seti,
            OpCode::Gtir,
            OpCode::Gtri,
            OpCode::Gtrr,
            OpCode::Eqir,
            OpCode::Eqri,
            OpCode::Eqrr,
        ]
        .iter()
    }
}

type Input = (Vec<(Vec<usize>, Vec<usize>, Vec<usize>)>, Vec<Vec<usize>>);

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn parse(input: &str) -> Input {
    let parts: Vec<_> = input.trim().split("\n\n\n\n").collect();
    let part1 = parts[0]
        .split("\n\n")
        .map(|l| {
            let l: Vec<_> = l.split('\n').collect();
            let a = l[0][9..19]
                .split(", ")
                .map(|x| x.parse().unwrap())
                .collect();
            let b = l[1].split(' ').map(|x| x.parse().unwrap()).collect();
            let c = l[2][9..19]
                .split(", ")
                .map(|x| x.parse().unwrap())
                .collect();
            (a, b, c)
        })
        .collect();
    let part2 = parts[1]
        .split('\n')
        .map(|x| x.split(' ').map(|x| x.parse().unwrap()).collect())
        .collect();
    (part1, part2)
}

fn part_1((p1, _): &Input) -> usize {
    let mut total = 0;
    for (before, operation, after) in p1 {
        let a = operation[1];
        let b = operation[2];
        let c = operation[3];
        let like = OpCode::iter()
            .filter(|&op| {
                let mut data = before.clone();
                op.apply(a, b, c, &mut data);
                &data == after
            })
            .count();
        if like >= 3 {
            total += 1;
        }
    }
    total
}

fn part_2((p1, p2): &Input) -> usize {
    let mut possible: HashMap<_, HashSet<_>> = HashMap::with_capacity(16);
    for (before, operation, after) in p1 {
        let opcode = operation[0];
        let a = operation[1];
        let b = operation[2];
        let c = operation[3];
        for op in OpCode::iter() {
            let mut data = before.clone();
            op.apply(a, b, c, &mut data);
            if after != &data {
                possible
                    .entry(opcode)
                    .or_insert_with(|| OpCode::iter().collect())
                    .remove(op);
            }
        }
    }

    let mut known = HashMap::with_capacity(16);
    // This is a real mess because the borrow checker exists
    while !possible.is_empty() {
        let mut to_remove = Vec::with_capacity(16);
        for (&k, v) in &possible {
            if v.len() == 1 {
                to_remove.push(k);
            }
        }

        for k in to_remove {
            for &&op in possible.get(&k).unwrap() {
                known.insert(k, op);
            }
            for entry in possible.values_mut() {
                entry.remove(known.get(&k).unwrap());
            }
            possible.remove(&k);
        }
    }

    let mut data = vec![0; 4];
    for operation in p2 {
        let opcode = operation[0];
        let a = operation[1];
        let b = operation[2];
        let c = operation[3];
        known.get(&opcode).unwrap().apply(a, b, c, &mut data)
    }
    data[0]
}
