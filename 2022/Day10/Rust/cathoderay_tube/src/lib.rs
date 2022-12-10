type Input<'a> = Vec<Instruction>;

#[derive(Debug)]
pub enum Instruction {
    Noop,
    Addx(isize),
}

pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|x| {
            if x.starts_with('n') {
                Instruction::Noop
            } else {
                let (_, b) = x.split_once(' ').unwrap();
                Instruction::Addx(b.parse().unwrap())
            }
        })
        .collect()
}

pub fn part_1(input: &Input) -> isize {
    let mut x = 1;
    let mut clock = 0;
    let mut ans = 0;

    for ins in input {
        match ins {
            Instruction::Noop => {
                clock += 1;
                if clock % 40 == 20 {
                    ans += x * clock;
                }
            }
            Instruction::Addx(val) => {
                for _ in 0..2 {
                    clock += 1;
                    if clock % 40 == 20 {
                        ans += x * clock;
                    }
                }
                x += val;
            }
        }
    }

    ans
}

pub fn part_2(input: &Input) -> String {
    let mut x: isize = 1;
    let mut clock = 0;
    let mut screen = vec!['_'; 240];

    for ins in input {
        match ins {
            Instruction::Noop => {
                screen[clock] = if x.abs_diff((clock % 40) as isize) <= 1 {
                    '█'
                } else {
                    '░'
                };
                clock += 1;
            }
            Instruction::Addx(val) => {
                for _ in 0..2 {
                    screen[clock] = if x.abs_diff((clock % 40) as isize) <= 1 {
                        '█'
                    } else {
                        '░'
                    };
                    clock += 1;
                }
                x += val;
            }
        }
    }
    screen
        .chunks(40)
        .map(|row| row.iter().collect())
        .collect::<Vec<String>>()
        .join("\n")
}
