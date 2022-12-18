use rustc_hash::FxHashMap as HashMap;
use rustc_hash::FxHashSet as HashSet;
use std::collections::VecDeque;

type Input = (HashMap<String, usize>, HashMap<String, Vec<String>>);

pub fn parse(input: &str) -> Input {
    let mut flow_rates = HashMap::default();
    let mut connected = HashMap::default();
    for line in input.trim().lines() {
        let (a, b) = line.split_once("; ").unwrap();

        let (valve, flow_rate) = a[6..].split_once(" has flow rate=").unwrap();
        flow_rates.insert(valve.to_owned(), flow_rate.parse().unwrap());
        connected.insert(
            valve.to_owned(),
            b.splitn(4, ' ')
                .last()
                .unwrap()
                .split_once(' ')
                .unwrap()
                .1
                .split(", ")
                .map(|s| s.to_owned())
                .collect(),
        );
    }
    (flow_rates, connected)
}

fn get_paths(
    from: &String,
    connected: &HashMap<String, Vec<String>>,
    flow_rates: &HashMap<String, usize>,
) -> Vec<(String, usize)> {
    let mut queue = VecDeque::new();
    let mut dests = Vec::new();
    let mut seen = HashSet::default();
    queue.push_back((from, 0));

    while let Some((node, time)) = queue.pop_front() {
        for new_node in connected.get(node).unwrap() {
            if !seen.contains(new_node) {
                seen.insert(new_node.to_owned());
                queue.push_back((new_node, time + 1));

                if *flow_rates.get(new_node).unwrap() > 0 {
                    dests.push((new_node.to_owned(), time + 2));
                }
            }
        }
    }

    dests
}

fn get_pressure_released<const PART: usize>(
    flow_rates: &HashMap<String, usize>,
    connected: &HashMap<String, Vec<String>>,
) -> usize {
    fn get_pressure_released_worker<const PART: usize>(
        current_flow_rate: usize,
        current_pos: &String,
        current_time_left: usize,
        opened_valves: &HashSet<String>,
        other_pos: &String,
        other_time_spent: usize,
        best_flow_rate: usize,
        seen: &mut HashMap<String, Vec<(String, usize)>>,
        flow_rates: &HashMap<String, usize>,
        connected: &HashMap<String, Vec<String>>,
    ) -> usize {
        let mut best_flow_rate = best_flow_rate;
        let mut ans = current_flow_rate;
        if !seen.contains_key(current_pos) {
            seen.insert(
                current_pos.to_owned(),
                get_paths(current_pos, connected, flow_rates),
            );
        }
        let p = seen.get(current_pos).unwrap().clone();
        for (dest, time) in p {
            if !opened_valves.contains(&dest) && time <= current_time_left {
                let new_flow_rate =
                    current_flow_rate + (current_time_left - time) * flow_rates.get(&dest).unwrap();
                let optimistic_remaining = (current_time_left * 2 - time - other_time_spent + 3)
                    * if PART == 1 {
                        (current_time_left - time + 3) * 60
                    } else {
                        40
                    };
                if optimistic_remaining + new_flow_rate < best_flow_rate {
                    continue;
                }

                let mut new_opened = opened_valves.clone();
                new_opened.insert(dest.to_owned());

                let new_score = if other_time_spent < time && PART == 2 {
                    get_pressure_released_worker::<PART>(
                        new_flow_rate,
                        other_pos,
                        current_time_left - other_time_spent,
                        &new_opened,
                        &dest,
                        time - other_time_spent,
                        best_flow_rate,
                        seen,
                        flow_rates,
                        connected,
                    )
                } else {
                    get_pressure_released_worker::<PART>(
                        new_flow_rate,
                        &dest,
                        current_time_left - time,
                        &new_opened,
                        other_pos,
                        other_time_spent - time,
                        best_flow_rate,
                        seen,
                        flow_rates,
                        connected,
                    )
                };
                best_flow_rate = best_flow_rate.max(new_score).max(current_flow_rate);
                ans = ans.max(best_flow_rate);
            }
        }
        ans
    }
    get_pressure_released_worker::<PART>(
        0,
        &"AA".to_string(),
        if PART == 1 { 30 } else { 26 },
        &mut HashSet::default(),
        &"AA".to_string(),
        0,
        0,
        &mut HashMap::default(),
        flow_rates,
        connected,
    )
}

pub fn part_1((flow_rates, connected): &Input) -> usize {
    get_pressure_released::<1>(flow_rates, connected)
}

pub fn part_2((flow_rates, connected): &Input) -> usize {
    get_pressure_released::<2>(flow_rates, connected)
}
