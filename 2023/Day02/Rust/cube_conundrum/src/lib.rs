type Input<'a> = Vec<Game>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Game {
    pub id: usize,
    pub max_red: usize,
    pub max_green: usize,
    pub max_blue: usize,
}

impl Game {
    /// # Panics
    #[must_use]
    pub fn new(s: &str) -> Game {
        let (game_id, cubes) = s.split_once(": ").unwrap();
        let (max_red, max_green, max_blue) = cubes
            .split([',', ';'])
            .map(|c| match c.trim().split_once(' ').unwrap() {
                (x, "red") => (x.parse().unwrap(), 0, 0),
                (x, "green") => (0, x.parse().unwrap(), 0),
                (x, "blue") => (0, 0, x.parse().unwrap()),
                _ => unreachable!(),
            })
            .fold((0, 0, 0), |(r_m, g_m, b_m), (r, g, b)| {
                (r_m.max(r), g_m.max(g), b_m.max(b))
            });
        Game {
            id: game_id.split_once(' ').unwrap().1.parse().unwrap(),
            max_red,
            max_green,
            max_blue,
        }
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
    const MAX_ALLOWED_RED: usize = 12;
    const MAX_ALLOWED_GREEN: usize = 13;
    const MAX_ALLOWED_BLUE: usize = 14;
    input
        .iter()
        .filter_map(|game| {
            if game.max_red <= MAX_ALLOWED_RED
                && game.max_green <= MAX_ALLOWED_GREEN
                && game.max_blue <= MAX_ALLOWED_BLUE
            {
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
    input
        .iter()
        .map(|g| g.max_red * g.max_green * g.max_blue)
        .sum()
}
