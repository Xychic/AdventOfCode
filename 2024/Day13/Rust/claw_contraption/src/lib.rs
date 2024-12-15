type Input<'a> = Vec<Machine>;
type Point = (isize, isize);

#[derive(Debug)]
pub struct Machine {
    a: Point,
    b: Point,
    prize: Point,
}

impl Machine {
    fn get_cost(&self) -> Option<usize> {
        let a_presses = ((self.prize.0 * self.b.1) - (self.prize.1 * self.b.0))
            / ((self.a.0 * self.b.1) - (self.a.1 * self.b.0));
        let b_presses = (self.prize.0 - (self.a.0 * a_presses)) / self.b.0;
        if (
            self.a.0 * a_presses + self.b.0 * b_presses,
            self.a.1 * a_presses + self.b.1 * b_presses,
        ) == self.prize
        {
            Some((3 * a_presses + b_presses).try_into().unwrap())
        } else {
            None
        }
    }
}

/// Parser for 2024 Day 13 (`claw_contraption`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .split("\n\n")
        .map(|s| {
            let mut lines = s.lines();
            let (a_x, a_y) = lines.next().unwrap().split_once(", ").unwrap();
            let (b_x, b_y) = lines.next().unwrap().split_once(", ").unwrap();
            let (prize_x, prize_y) = lines.next().unwrap().split_once(", ").unwrap();
            Machine {
                a: (a_x[12..].parse().unwrap(), a_y[2..].parse().unwrap()),
                b: (b_x[12..].parse().unwrap(), b_y[2..].parse().unwrap()),
                prize: (prize_x[9..].parse().unwrap(), prize_y[2..].parse().unwrap()),
            }
        })
        .collect()
}

/// Solver for part 1 of 2024 Day 13 (`claw_contraption`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input.iter().filter_map(Machine::get_cost).sum()
}

/// Solver for part 2 of 2024 Day 13 (`claw_contraption`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    input
        .iter()
        .map(|m| Machine {
            a: m.a,
            b: m.b,
            prize: (
                10_000_000_000_000 + m.prize.0,
                10_000_000_000_000 + m.prize.1,
            ),
        })
        .filter_map(|m| m.get_cost())
        .sum()
}
