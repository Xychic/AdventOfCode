use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap},
    fs,
    time::Instant,
};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct State {
    pos: (usize, usize),
    cost: usize,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.pos.cmp(&other.pos))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

type Input<'a> = Vec<Vec<usize>>;

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
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect()
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    path(input, (0, 0), (input[0].len() - 1, input.len() - 1))
}

fn part_2(input: &Input) -> usize {
    let max_y = input.len();
    let max_x = input[0].len();
    let mut grid = vec![vec![0; max_x * 5]; max_y * 5];

    for y in 0..max_y * 5 {
        for x in 0..max_x * 5 {
            let mut val = input[y % max_y][x % max_x] + (y / max_y) + (x / max_x);
            if val > 9 {
                val = (val % 10) + 1;
            }
            grid[y][x] = val;
        }
    }

    path(&grid, (0, 0), (max_x * 5 - 1, max_y * 5 - 1))
}

fn path(grid: &Vec<Vec<usize>>, start: (usize, usize), end: (usize, usize)) -> usize {
    let max_y = grid.len();
    let max_x = grid[0].len();

    let mut heap: BinaryHeap<State> = BinaryHeap::new();
    let mut distances: HashMap<(usize, usize), usize> = HashMap::new();

    distances.insert(start, 0);
    heap.push(State {
        pos: start,
        cost: 0,
    });
    while let Some(State { pos, cost }) = heap.pop() {
        if pos == end {
            return cost;
        }
        if let Some(best_cost) = distances.get(&pos) {
            if &cost > best_cost {
                continue;
            }
        }

        let (x, y) = pos;
        for (new_x, new_y) in [
            (x.wrapping_sub(1), y),
            (x + 1, y),
            (x, y.wrapping_sub(1)),
            (x, y + 1),
        ] {
            if new_x >= max_x || new_y >= max_y {
                continue;
            }
            let next = State {
                pos: (new_x, new_y),
                cost: cost + grid[new_y][new_x],
            };

            if &next.cost < distances.get(&(new_x, new_y)).unwrap_or(&usize::MAX) {
                heap.push(next);
                distances.insert(next.pos, next.cost);
            }
        }
    }
    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 40);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 315);
    }
}
