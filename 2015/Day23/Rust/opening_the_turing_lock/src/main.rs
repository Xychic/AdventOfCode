use std::{collections::HashMap, fs, time::Instant};

type Input<'a> = Vec<Instruction<'a>>;

struct State<'a> {
    registers: HashMap<&'a str, usize>,
    pc: isize,
}
impl<'a> State<'a> {
    fn new() -> State<'a> {
        State {
            registers: HashMap::new(),
            pc: 0,
        }
    }
}

#[derive(Debug)]
enum Instruction<'a> {
    Hlf { register: &'a str },
    Tpl { register: &'a str },
    Inc { register: &'a str },
    Jmp { offset: isize },
    Jie { register: &'a str, offset: isize },
    Jio { register: &'a str, offset: isize },
}

impl<'a> Instruction<'a> {
    fn from_string(string: &'a str) -> Instruction<'a> {
        let (instruction, args) = string.split_once(' ').unwrap();
        match instruction {
            "hlf" => Instruction::Hlf { register: args },
            "tpl" => Instruction::Tpl { register: args },
            "inc" => Instruction::Inc { register: args },
            "jmp" => Instruction::Jmp {
                offset: args.parse().unwrap(),
            },
            "jie" => {
                let (register, offset) = args.split_once(", ").unwrap();
                Instruction::Jie {
                    register,
                    offset: offset.parse().unwrap(),
                }
            }
            "jio" => {
                let (register, offset) = args.split_once(", ").unwrap();
                Instruction::Jio {
                    register,
                    offset: offset.parse().unwrap(),
                }
            }
            _otherwise => unreachable!(),
        }
    }

    fn step(&self, state: &mut State<'a>) {
        match self {
            Instruction::Hlf { register } => {
                let current_val = state.registers.get(register).unwrap_or(&0);
                state.registers.insert(register, current_val / 2);
                state.pc += 1;
            }
            Instruction::Tpl { register } => {
                let current_val = *state.registers.get(register).unwrap_or(&0);
                state.registers.insert(register, current_val * 3);
                state.pc += 1;
            }
            Instruction::Inc { register } => {
                let current_val = state.registers.get(register).unwrap_or(&0);
                state.registers.insert(register, current_val + 1);
                state.pc += 1;
            }
            Instruction::Jmp { offset } => {
                state.pc += offset;
            }
            Instruction::Jie { register, offset } => {
                let current_val = state.registers.get(register).unwrap_or(&0);
                if current_val % 2 == 0 {
                    state.pc += offset;
                } else {
                    state.pc += 1;
                }
            }
            Instruction::Jio { register, offset } => {
                let current_val = state.registers.get(register).unwrap_or(&0);
                if current_val == &1 {
                    state.pc += offset;
                } else {
                    state.pc += 1;
                }
            }
        }
    }
}

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    let start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1(&input),
        Instant::now() - start
    );
    let start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2(&input),
        Instant::now() - start
    );
}

fn parse(input: &str) -> Input {
    input
        .trim()
        .split('\n')
        .map(Instruction::from_string)
        .collect()
}

fn part_1(input: &Input) -> usize {
    let mut state = State::new();
    while (state.pc as usize) < input.len() {
        let instruction = &input[state.pc as usize];
        instruction.step(&mut state);
    }
    *state.registers.get(&"b").unwrap_or(&0)
}

fn part_2(input: &Input) -> usize {
    let mut state = State::new();
    state.registers.insert("a", 1);
    while (state.pc as usize) < input.len() {
        let instruction = &input[state.pc as usize];
        instruction.step(&mut state);
    }
    *state.registers.get(&"b").unwrap_or(&0)
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "inc b
jio b, +2
tpl b
inc b";

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 2);
    }
}
