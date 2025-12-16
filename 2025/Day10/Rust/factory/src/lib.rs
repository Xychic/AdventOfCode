use std::collections::{HashMap, HashSet, VecDeque};

type Input = Vec<Machine>;

#[derive(Debug, Clone)]
pub struct Machine {
    pub target: u32,
    pub buttons: Vec<u32>,
    pub joltage: Vec<u32>,
}

/// Parser for 2025 Day 10 (`factory`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let mut parts = l.split(' ');
            let target = parts
                .next()
                .unwrap()
                .chars()
                .filter_map(|c| match c {
                    '#' => Some(1),
                    '.' => Some(0),
                    _ => None,
                })
                .rev()
                .reduce(|acc, x| (acc * 2) + x)
                .unwrap();
            let buttons: Vec<_> = parts
                .map(|b| {
                    b[1..b.len() - 1]
                        .split(',')
                        .map(|c| c.parse().unwrap())
                        .collect::<Vec<_>>()
                })
                .collect();
            Machine {
                target,
                buttons: buttons[..buttons.len() - 1]
                    .iter()
                    .map(|xs| xs.iter().map(|&x| 2_u32.pow(x)).sum())
                    .collect(),
                joltage: buttons.last().unwrap().clone(),
            }
        })
        .collect()
}

fn solve_p1(m: &Machine) -> usize {
    let mut queue = VecDeque::new();
    let mut seen = HashSet::new();

    queue.push_back((0, 0));
    while let Some((state, steps)) = queue.pop_front() {
        if state == m.target {
            return steps;
        }
        seen.insert(state);

        for buttons in &m.buttons {
            let new_state = state ^ buttons;

            if seen.contains(&new_state) {
                continue;
            }
            queue.push_back((new_state, steps + 1));
        }
    }
    unreachable!()
}

/// Solver for part 1 of 2025 Day 10 (`factory`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    input.iter().map(solve_p1).sum()
}

/// Solver for part 2 of 2025 Day 10 (`factory`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    input.iter().map(solve_p2).sum()
}

fn solve_p2(m: &Machine) -> usize {
    let options = buttons_options(&m.buttons, vec![0_u32; m.joltage.len()], 0);
    solve_inner(&m.joltage, &options, &mut HashMap::new())
}

fn solve_inner(
    joltage: &[u32],
    options: &[(Vec<u32>, usize)],
    cache: &mut HashMap<Vec<u32>, usize>,
) -> usize {
    if let Some(&ans) = cache.get(joltage) {
        return ans;
    }

    if joltage.iter().all(|&j| j == 0) {
        cache.insert(joltage.to_vec(), 0);
        return 0;
    }

    let ans = options
        .iter()
        .map(|(delta, presses)| {
            if delta
                .iter()
                .zip(joltage)
                .all(|(i, j)| i <= j && i % 2 == j % 2)
            {
                presses.saturating_add(
                    2_usize.saturating_mul(solve_inner(
                        &joltage
                            .iter()
                            .zip(delta)
                            .map(|(i, d)| (i - d) / 2)
                            .collect::<Vec<_>>(),
                        options,
                        cache,
                    )),
                )
            } else {
                usize::MAX
            }
        })
        .min()
        .unwrap();

    cache.insert(joltage.to_vec(), ans);

    ans
}

fn buttons_options(buttons: &[u32], state: Vec<u32>, presses: usize) -> Vec<(Vec<u32>, usize)> {
    if buttons.is_empty() {
        return vec![(state, presses)];
    }
    let mut ans = buttons_options(&buttons[1..], state.clone(), presses);
    ans.extend(
        buttons_options(
            &buttons[1..],
            state
                .iter()
                .enumerate()
                .map(|(i, &x)| {
                    if (buttons[0] & 2_u32.pow(u32::try_from(i).unwrap())) != 0 {
                        x + 1
                    } else {
                        x
                    }
                })
                .collect(),
            presses + 1,
        )
        .iter()
        .cloned(),
    );
    ans
}
