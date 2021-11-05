use std::fs;

#[derive(Debug, Clone)]
struct Instruction {
    operation: String,
    parameters: (usize, usize),
}

fn main() {
    let input: String = fs::read_to_string("../../../input.txt").expect("error reading file");
    let lines: Vec<&str> = input.trim().split("\n").collect();
    let parsed = parse_input(lines);

    println!("Part 1: {}", part_1(parsed.clone()));
}

fn part_1(instructions: Vec<Instruction>) -> usize {
    let mut screen = vec![vec![false; 50]; 6];
    for ins in instructions {
        if ins.operation == "rect" {
            for x in 0..ins.parameters.0 {
                for y in 0..ins.parameters.1 {
                    screen[y][x] = true;
                }
            }
        } else if ins.operation == "rotate:y" {
            let to_rotate = screen[ins.parameters.0].clone();
            let size = to_rotate.len();
            for x in 0..size { screen[ins.parameters.0][(x+ins.parameters.1) % size] = to_rotate[x]; }
        } else if ins.operation == "rotate:x" {
            let mut to_rotate: Vec<bool> = Vec::new();
            let size: usize = screen.len();
            for y in 0..size { to_rotate.push(screen[y][ins.parameters.0]); }
            for y in 0..size { screen[(y+ins.parameters.1) % size][ins.parameters.0] = to_rotate[y]; }
        }
    }
    println!("{}",screen_to_string(screen.clone()));
    screen_to_string(screen).matches('#').count()
}

fn parse_input(lines: Vec<&str>) -> Vec<Instruction> {
    lines
    .iter()
    .map(|line| {
        let data = 
        line
        .split(" ")
        .collect::<Vec<&str>>();

        if data[0] == "rect" {
            let params = 
                data[1]
                .split('x')
                .map(|c| {
                    c
                    .parse::<usize>()
                    .expect("NaN")
                })
                .collect::<Vec<usize>>();  

            let params = (params[0], params[1]);
            Instruction {operation: String::from("rect"), parameters: params}
        } else {
            let params = (
                data[2]
                .split("=")
                .collect::<Vec<&str>>()[1]
                .parse::<usize>()
                .expect("NaN")
                ,
                data[4]
                .parse::<usize>()
                .expect("Nan")
            );
            Instruction {operation: format!("rotate:{}", if data[1] == "row" {'y'} else {'x'}), parameters: params}
        }
    }).collect::<Vec<Instruction>>()
}

fn screen_to_string(screen: Vec<Vec<bool>>) -> String {
    let mut screen_string = String::new();
    for row in screen {
        for pixel in row {
            screen_string.push(if pixel {'#'} else {' '});
        }
        screen_string.push('\n');
    }
    screen_string
}
