type Input<'a> = ((usize, usize, usize), Vec<usize>);

/// Parser for 2024 Day 17 (`chronospatial_computer`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    let (registers, instructions) = input.trim().split_once("\n\n").unwrap();
    let mut registers = registers.lines().map(|l| l[12..].parse().unwrap());
    (
        (
            registers.next().unwrap(),
            registers.next().unwrap(),
            registers.next().unwrap(),
        ),
        instructions[9..]
            .split(',')
            .map(|x| x.parse().unwrap())
            .collect(),
    )
}

/// Solver for part 1 of 2024 Day 17 (`chronospatial_computer`)
///
/// # Panics
#[must_use]
pub fn part_1((registers, instructions): &Input) -> String {
    let (mut a, mut b, mut c) = registers;
    let mut ip = 0;
    let mut out = Vec::new();
    while ip < instructions.len() {
        // println!("{a} {b} {c} {ip}, {}", instructions.len());
        let combo_operand = match instructions.get(ip + 1).unwrap() {
            0 => 0,
            1 => 1,
            2 => 2,
            3 => 3,
            4 => a,
            5 => b,
            6 => c,
            _ => unreachable!(),
        };
        match instructions.get(ip).unwrap() {
            0 => a /= 2_usize.pow(u32::try_from(combo_operand).unwrap()),
            1 => b ^= instructions.get(ip + 1).unwrap(),
            2 => b = combo_operand % 8,
            3 => {
                if a != 0 {
                    ip = instructions[ip + 1] - 2;
                }
            }
            4 => b ^= c,
            5 => out.push((combo_operand % 8).to_string()),
            6 => b = a / 2_usize.pow(u32::try_from(combo_operand).unwrap()),
            7 => c = a / 2_usize.pow(u32::try_from(combo_operand).unwrap()),
            _ => unreachable!(),
        }
        ip += 2;
    }
    out.join(",")
}

/// Solver for part 2 of 2024 Day 17 (`chronospatial_computer`)
///
/// # Panics
#[must_use]
pub fn part_2((_, instructions): &Input) -> usize {
    fn solve(target: &[usize], ans: usize) -> Option<usize> {
        if let Some(&next) = target.last() {
            (0..8).find_map(|x| {
                let a = (ans << 3) + x;
                let mut b = a & 7;
                b ^= 2;
                let c = a >> b;
                b ^= c;
                b ^= 3;
                if b & 7 == next {
                    solve(&target[..target.len() - 1], a)
                } else {
                    None
                }
            })
        } else {
            Some(ans)
        }
    }
    solve(instructions, 0).unwrap()
}
