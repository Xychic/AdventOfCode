use std::collections::VecDeque;

type Input = VecDeque<isize>;

fn modulo(a: isize, b: isize) -> isize {
    ((a % b) + b) % b
}

pub fn parse(input: &str) -> Input {
    input.trim().lines().map(|l| l.parse().unwrap()).collect()
}

fn solve<const PART: usize>(input: &Input) -> isize {
    assert!(PART == 1 || PART == 2);

    let mut input: VecDeque<_> = input
        .iter()
        .enumerate()
        .map(|(i, &x)| (i as isize, x * if PART == 1 { 1 } else { 811589153 }))
        .collect();

    for _ in 0..if PART == 1 { 1 } else { 10 } {
        for i in 0..input.len() as isize {
            while input[0].0 != i {
                let tmp = input.pop_front().unwrap();
                input.push_back(tmp)
            }

            let num @ (_, index) = input.pop_front().unwrap();
            for _ in 0..modulo(index, input.len() as isize) {
                let tmp = input.pop_front().unwrap();
                input.push_back(tmp);
            }
            input.push_back(num);
        }
    }

    let mut index_0 = 0;
    while input[index_0].1 != 0 {
        index_0 += 1;
    }
    input[(index_0 + 1000) % input.len() as usize].1
        + input[(index_0 + 2000) % input.len() as usize].1
        + input[(index_0 + 3000) % input.len() as usize].1
}

pub fn part_1(input: &Input) -> isize {
    solve::<1>(input)
}

pub fn part_2(input: &Input) -> isize {
    solve::<2>(input)
}
