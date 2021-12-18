use std::{fs, time::Instant};

use itertools::Itertools;

type Input = Vec<Vec<(usize, usize)>>;

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
            let mut data = Vec::with_capacity(l.len());
            let mut depth = 0;
            for c in l.chars() {
                match c {
                    '[' => depth += 1,
                    ']' => depth -= 1,
                    ',' => continue,
                    _ => data.push((c.to_digit(10).unwrap() as usize, depth)),
                }
            }
            data
        })
        .collect()
}

fn part_1(input: &Input) -> usize {
    magnitude(
        &mut input
            .iter()
            .skip(1)
            .fold(input[0].to_owned(), |acc, x| add(&acc, x)),
    )
}

fn part_2(input: &Input) -> usize {
    input
        .iter()
        .permutations(2)
        .map(|x| magnitude(&mut add(x[0], x[1])))
        .max()
        .unwrap()
}

fn add(a: &Vec<(usize, usize)>, b: &Vec<(usize, usize)>) -> Vec<(usize, usize)> {
    let mut ans = Vec::with_capacity(a.len() + b.len());
    for arr in [a, b] {
        for (val, depth) in arr {
            ans.push((*val, depth + 1));
        }
    }
    loop {
        if !(explode(&mut ans) || split(&mut ans)) {
            break;
        }
    }
    ans
}

fn split(arr: &mut Vec<(usize, usize)>) -> bool {
    let mut data = None;
    for (i, (v, d)) in arr.iter().enumerate() {
        if *v >= 10 {
            data = Some((*v, *d, i));
            break;
        }
    }
    if let Some((value, depth, index)) = data {
        let a = value / 2;
        let b = (value + 1) / 2;
        arr[index] = (a, depth + 1);
        arr.insert(index + 1, (b, depth + 1));

        return true;
    }
    false
}

fn explode(arr: &mut Vec<(usize, usize)>) -> bool {
    let mut data = None;
    for (i, (v, d)) in arr.iter().enumerate() {
        if *d > 4 {
            data = Some((*v, *d, i));
            break;
        }
    }
    if let Some((value, depth, index)) = data {
        if index > 0 {
            let (a, b) = arr[index - 1];
            arr[index - 1] = (value + a, b);
        }
        if index < arr.len() - 2 {
            let (a1, _) = arr[index + 1];
            let (a2, b2) = arr[index + 2];
            arr[index + 2] = (a1 + a2, b2);
        }
        arr[index] = (0, depth - 1);
        arr.remove(index + 1);

        return true;
    }
    false
}

fn magnitude(arr: &mut Vec<(usize, usize)>) -> usize {
    'outer: loop {
        for i in 0..arr.len() - 1 {
            if arr[i].1 == arr[i + 1].1 {
                arr[i] = (3 * arr[i].0 + 2 * arr[i + 1].0, arr[i].1 - 1);
                arr.remove(i + 1);
                continue 'outer;
            }
        }
        break;
    }
    arr[0].0
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 4140);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 3993);
    }
}
