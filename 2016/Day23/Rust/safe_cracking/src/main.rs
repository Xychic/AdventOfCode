use std::{cmp, collections::HashMap, fs};

#[derive(Debug, Clone)]
enum Instruction {
    CPY { a: String, b: String },
    INC { a: String },
    DEC { a: String },
    JNZ { a: String, b: String },
    TGL { a: String }
}

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse_input(input.trim());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn part_1(instructions: &Vec<Instruction>) -> isize {
    run_machine(instructions, 7)
}

fn part_2(instructions: &Vec<Instruction>) -> isize {
    run_machine(instructions, 7) + (factorial(12) - factorial(7)) as isize
}

fn run_machine(instructions: &Vec<Instruction>, starting_val: isize) -> isize {
    let mut instructions = instructions.clone();
    let mut data: HashMap<String, isize> = HashMap::new();
    let mut index = 0;
    data.insert("a".to_string(), starting_val);

    while index < instructions.len() {
        let instrcuctions_copy = instructions.clone();
        let instruction = &instrcuctions_copy[index];
        match instruction {
            Instruction::CPY{a, b} => { apply_copy(a, b, &mut data, &mut index); }
            Instruction::INC{a} => { apply_increment(a, &mut data, &mut index); }
            Instruction::DEC{a} => { apply_decrement(a, &mut data, &mut index); }
            Instruction::JNZ{a, b} => { apply_jump_not_zero(a, b, &mut data, &mut index); }
            Instruction::TGL{a} => { apply_toggle(a, &mut data, &mut index, &mut instructions); }
        }
    }
    *data.get("a").unwrap()
}

fn parse_input(input: &str) -> Vec<Instruction> {
    input
    .split('\n')
    .map(|line| {
        let data: Vec<_> = line.split(' ').collect();
        match data[..] {
            ["cpy", a, b] => { return Instruction::CPY{a: a.to_string(), b: b.to_string()}; }
            ["inc", a] => { return Instruction::INC{a: a.to_string()}; }
            ["dec", a] => { return Instruction::DEC{a: a.to_string()}; }
            ["jnz", a, b] => { return Instruction::JNZ{a: a.to_string(), b: b.to_string()}; }
            ["tgl", a] => { return Instruction::TGL{a: a.to_string()}; }
            _ => {panic!("Unknown Instruction {:?}", data);}
        }
    })
    .collect()
}

fn is_number(string: &str) -> bool {
    string.chars().all(|c| c.is_numeric() || c == '-')
}

fn apply_copy(a: &str, b: &str, data: &mut HashMap<String, isize>, index: &mut usize) {
    let val = if is_number(a) {a.parse().unwrap()} else {*data.get(a).unwrap_or(&0)};
    if ! is_number(b) {
        data.insert(b.to_string(), val);
    }
    *index += 1;
}

fn apply_increment(a: &str, data: &mut HashMap<String, isize>, index: &mut usize) {
    if ! is_number(a) {
        let val = data.get(a).unwrap_or(&0) + 1;
        data.insert(a.to_string(), val);
    }
    *index += 1;
}

fn apply_decrement(a: &str, data: &mut HashMap<String, isize>, index: &mut usize) {
    if ! is_number(a) {
        let val = cmp::max(data.get(a).unwrap_or(&0) - 1, 0);
        data.insert(a.to_string(), val);
    }
    *index += 1;
}

fn apply_jump_not_zero(a: &str, b: &str, data: &mut HashMap<String, isize>, index: &mut usize) {
    let val_a = if is_number(a) {a.parse().unwrap()} else {*data.get(a).unwrap_or(&0)};
    let val_b = if is_number(b) {b.parse().unwrap()} else {*data.get(b).unwrap_or(&0)};
    let new_index = *index as isize + if val_a != 0 {val_b} else {1};
    *index = new_index as usize;
}

fn apply_toggle(a: &str, data: &mut HashMap<String, isize>, index: &mut usize, instructions: &mut Vec<Instruction>) {
    let val_a = if is_number(a) {a.parse().unwrap()} else {*data.get(a).unwrap_or(&0)};
    let target_index = (*index as isize + val_a) as usize;
    if target_index < instructions.len() {
        let ins = &instructions[target_index];
        let new_ins = match ins {
            Instruction::INC{ a } => Instruction::DEC{ a: a.to_string() },
            Instruction::DEC { a } => Instruction::INC{ a: a.to_string() },
            Instruction::TGL { a } => Instruction::INC{ a: a.to_string() },
            Instruction::CPY { a, b } => Instruction::JNZ { a: a.to_string(), b: b.to_string() },
            Instruction::JNZ { a, b } => Instruction::CPY { a: a.to_string().to_string(), b: b.to_string() }
        };
        instructions[target_index] = new_ins;
    }
    *index += 1;
}

fn factorial(n: usize) -> usize {
    match n {
        0 => 1,
        1 => 1,
        _ => n * factorial(n-1)
    }
}