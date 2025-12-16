use std::collections::HashMap;

type Input<'a> = HashMap<&'a str, Vec<&'a str>>;

/// Parser for 2025 Day 11 (`reactor`)
///
/// # Panics
#[must_use]
pub fn parse(input: &'_ str) -> Input<'_> {
    input
        .trim()
        .lines()
        .map(|l| {
            let (from, to) = l.split_once(": ").unwrap();
            (from, to.split(' ').collect())
        })
        .collect()
}

fn get_path(
    pos: &str,
    target: &str,
    fft: bool,
    dac: bool,
    connections: &Input,
    seen: &mut HashMap<(String, bool, bool), usize>,
) -> usize {
    if let Some(&ans) = seen.get(&(pos.to_string(), fft, dac)) {
        return ans;
    }
    if pos == target {
        return usize::from(fft && dac);
    }
    let ans = connections.get(pos).map_or(0, |v| {
        v.iter()
            .map(|&next| {
                get_path(
                    next,
                    target,
                    fft || next == "fft",
                    dac || next == "dac",
                    connections,
                    seen,
                )
            })
            .sum::<usize>()
    });

    seen.insert((pos.to_string(), fft, dac), ans);

    ans
}

/// Solver for part 1 of 2025 Day 11 (`reactor`)
///
/// # Panics
#[must_use]
pub fn part_1(input: &Input) -> usize {
    get_path("you", "out", true, true, input, &mut HashMap::new())
}

/// Solver for part 2 of 2025 Day 11 (`reactor`)
///
/// # Panics
#[must_use]
pub fn part_2(input: &Input) -> usize {
    get_path("svr", "out", false, false, input, &mut HashMap::new())
}
