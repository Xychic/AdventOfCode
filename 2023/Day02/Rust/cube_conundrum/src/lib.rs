type Input<'a> = Vec<Game>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cube {
    Red,
    Green,
    Blue,
}

impl Cube {
    #[must_use]
    pub fn max(&self) -> usize {
        match self {
            Cube::Red => 12,
            Cube::Green => 13,
            Cube::Blue => 14,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Game {
    pub id: usize,
    pub cubes: Vec<(Cube, usize)>,
}

impl Game {
    /// # Panics
    #[must_use]
    pub fn new(s: &str) -> Game {
        let (game_id, cubes) = s.split_once(": ").unwrap();
        Game {
            id: game_id.split_once(' ').unwrap().1.parse().unwrap(),
            cubes: cubes
                .split([',', ';'])
                .map(|c| {
                    let (count, colour) = c.trim().split_once(' ').unwrap();
                    (
                        match colour {
                            "red" => Cube::Red,
                            "green" => Cube::Green,
                            "blue" => Cube::Blue,
                            _ => unreachable!("Invalid colour: {colour}"),
                        },
                        count.parse().unwrap(),
                    )
                })
                .collect(),
        }
    }

    #[must_use]
    pub fn power(&self) -> usize {
        self.cubes
            .iter()
            .fold([1, 1, 1], |[r, g, b], (cube, count)| match cube {
                Cube::Red => [r.max(*count), g, b],
                Cube::Green => [r, g.max(*count), b],
                Cube::Blue => [r, g, b.max(*count)],
            })
            .iter()
            .product()
    }
}

/// Parser for 2023 Day 02 (`cube_conundrum`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input.trim().lines().map(Game::new).collect()
}

/// Solver for part 1 of 2023 Day 02 (`cube_conundrum`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input
        .iter()
        .filter_map(|game| {
            if game.cubes.iter().all(|(c, count)| *count <= c.max()) {
                Some(game.id)
            } else {
                None
            }
        })
        .sum()
}

/// Solver for part 2 of 2023 Day 02 (`cube_conundrum`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    input.iter().map(Game::power).sum()
}
