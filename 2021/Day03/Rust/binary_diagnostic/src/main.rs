use std::{fs, time::Instant};

type Input = Vec<Vec<usize>>;

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
        .split('\n')
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    '0' => 0,
                    _ => 1,
                })
                .collect()
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    let size = input[0].len();
    let mut gamma = Vec::with_capacity(size);
    let mut epsilon = Vec::with_capacity(size);
    let mut count;

    for i in 0..size {
        count = [0; 2];
        for line in input {
            count[line[i]] += 1;
        }
        if count[0] > count[1] {
            gamma.push(0);
            epsilon.push(1);
        } else {
            gamma.push(1);
            epsilon.push(0);
        }
    }
    bin_to_dec(&gamma) * bin_to_dec(&epsilon)
}

fn part_2(input: &Input) -> usize {
    let mut o = input.to_owned();
    let mut c = input.to_owned();

    let mut count;
    for i in 0..input[0].len() {
        let o_size = o.len();
        let c_size = c.len();
        if o_size == 1 && c_size == 1 {
            break;
        }
        if o_size > 1 {
            count = [0; 2];
            for line in &o {
                count[line[i]] += 1;
            }
            o.retain(|line| line[i] == if count[0] > count[1] { 0 } else { 1 });
        }
        if c_size > 1 {
            count = [0; 2];
            for line in &c {
                count[line[i]] += 1;
            }
            c.retain(|line| line[i] == if count[0] <= count[1] { 0 } else { 1 });
        }
    }

    bin_to_dec(&o[0]) * bin_to_dec(&c[0])
}

fn bin_to_dec(bin_array: &[usize]) -> usize {
    let mut ans = 0;
    let mut pow = 1;
    for d in bin_array.iter().rev() {
        ans += d * pow;
        pow *= 2;
    }
    ans
}
