use std::{cmp, collections::{HashMap}, fs};

#[derive(Debug, Clone)]
enum Instruction {
    Cpy { a: String, b: String },
    Inc { a: String },
    Dec { a: String },
    Jnz { a: String, b: String },
    Tgl { a: String },
    Out { a: String }
}
fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse_input(input.trim());

    println!("Part 1: {}", part_1(&input));
}

fn part_1(instructions: &[Instruction]) -> isize {
    for i in 1.. {
        if run_machine(instructions, i) {
            return i;
        }
    }
    panic!()
}

fn run_machine(instructions: &[Instruction], starting_val: isize) -> bool {
    let mut instructions = instructions.to_owned();
    let mut data: HashMap<String, isize> = HashMap::new();
    let mut index = 0;
    data.insert("a".to_string(), starting_val);
    let mut order = Vec::new();

    while index < instructions.len() {
        let instrcuctions_copy = instructions.clone();
        let instruction = &instrcuctions_copy[index];
        match instruction {
            Instruction::Cpy{a, b} => { apply_copy(a, b, &mut data, &mut index); }
            Instruction::Inc{a} => { apply_increment(a, &mut data, &mut index); }
            Instruction::Dec{a} => { apply_decrement(a, &mut data, &mut index); }
            Instruction::Jnz{a, b} => { apply_jump_not_zero(a, b, &mut data, &mut index); }
            Instruction::Tgl{a} => { apply_toggle(a, &mut data, &mut index, &mut instructions); }
            Instruction::Out{a} => { order.push(get_data(a, &data)); index += 1; }
        }
        
        if !order.iter().zip(order.iter().skip(1)).all(|(a, b)| a != b) {
            return false;
        }
        if order.len() > 16 { return true; }
    }
    false
}

fn parse_input(input: &str) -> Vec<Instruction> {
    input
    .split('\n')
    .map(|line| {
        let data: Vec<_> = line.split(' ').collect();
        match data[..] {
            ["cpy", a, b] => { Instruction::Cpy{a: a.to_string(), b: b.to_string()} }
            ["inc", a] => { Instruction::Inc{a: a.to_string()} }
            ["dec", a] => { Instruction::Dec{a: a.to_string()} }
            ["jnz", a, b] => { Instruction::Jnz{a: a.to_string(), b: b.to_string()} }
            ["tgl", a] => { Instruction::Tgl{a: a.to_string()} }
            ["out", a] => { Instruction::Out{a: a.to_string()} }
            _ => {panic!("Unknown Instruction {:?}", data);}
        }
    })
    .collect()
}

fn is_number(string: &str) -> bool {
    string.chars().all(|c| c.is_numeric() || c == '-')
}

fn get_data(string: &str, data: &HashMap<String, isize>) -> isize {
    if is_number(string) {string.parse().unwrap()} else {*data.get(string).unwrap_or(&0)}
}

fn apply_copy(a: &str, b: &str, data: &mut HashMap<String, isize>, index: &mut usize) {
    let val = get_data(a, data);
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
    let val_a = get_data(a, data);
    let val_b = get_data(b, data);
    let new_index = *index as isize + if val_a != 0 {val_b} else {1};
    *index = new_index as usize;
}

fn apply_toggle(a: &str, data: &mut HashMap<String, isize>, index: &mut usize, instructions: &mut Vec<Instruction>) {
    let val_a = get_data(a, data);
    let target_index = (*index as isize + val_a) as usize;
    if target_index < instructions.len() {
        let ins = &instructions[target_index];
        let new_ins = match ins {
            Instruction::Inc{ a } => Instruction::Dec{ a: a.to_string() },
            Instruction::Dec { a } => Instruction::Inc{ a: a.to_string() },
            Instruction::Tgl { a } => Instruction::Inc{ a: a.to_string() },
            Instruction::Out { a } => Instruction::Inc{ a: a.to_string() },
            Instruction::Cpy { a, b } => Instruction::Jnz { a: a.to_string(), b: b.to_string() },
            Instruction::Jnz { a, b } => Instruction::Cpy { a: a.to_string(), b: b.to_string() }
        };
        instructions[target_index] = new_ins;
    }
    *index += 1;
}
