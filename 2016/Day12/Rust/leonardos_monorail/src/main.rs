use std::{cmp, collections::HashMap, fs, hash::Hash};

#[derive(Debug)]
enum Instruction {
    CPY { a: String, b: String },
    INC { a: String },
    DEC { a: String },
    JNZ { a:String, b: String }

}
fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse_input(input.trim());

    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
}

fn part_1(instructions: &Vec<Instruction>) -> isize {
    let mut data: HashMap<String, isize> = HashMap::new();
    let mut index = 0;
    while index < instructions.len() {
        let instruction = &instructions[index];
        match instruction {
            Instruction::CPY{a, b} => { apply_copy(a, b, &mut data, &mut index); }
            Instruction::INC{a} => { apply_increment(a, &mut data, &mut index); }
            Instruction::DEC{a} => { apply_decrement(a, &mut data, &mut index); }
            Instruction::JNZ{a, b} => { apply_jump_not_zero(a, b, &mut data, &mut index); }
        }
    }
    *data.get("a").unwrap()
}

fn part_2(instructions: &Vec<Instruction>) -> isize {
    let mut data: HashMap<String, isize> = HashMap::new();
    let mut index = 0;
    data.insert("c".to_string(), 1);
    while index < instructions.len() {
        let instruction = &instructions[index];
        match instruction {
            Instruction::CPY{a, b} => { apply_copy(a, b, &mut data, &mut index); }
            Instruction::INC{a} => { apply_increment(a, &mut data, &mut index); }
            Instruction::DEC{a} => { apply_decrement(a, &mut data, &mut index); }
            Instruction::JNZ{a, b} => { apply_jump_not_zero(a, b, &mut data, &mut index); }
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
    data.insert(b.to_string(), val);
    *index += 1;
}

fn apply_increment(a: &str, data: &mut HashMap<String, isize>, index: &mut usize) {
    let val = data.get(a).unwrap_or(&0) + 1;
    data.insert(a.to_string(), val);
    *index += 1;
}

fn apply_decrement(a: &str, data: &mut HashMap<String, isize>, index: &mut usize) {
    let val = cmp::max(data.get(a).unwrap_or(&0) - 1, 0);
    data.insert(a.to_string(), val);
    *index += 1;
}

fn apply_jump_not_zero(a: &str, b: &str, data: &mut HashMap<String, isize>, index: &mut usize) {
    let val_a = if is_number(a) {a.parse().unwrap()} else {*data.get(a).unwrap_or(&0)};
    let val_b = if is_number(b) {b.parse().unwrap()} else {*data.get(b).unwrap_or(&0)};
    let new_index = *index as isize + if val_a != 0 {val_b} else {1};
    *index = new_index as usize;
}