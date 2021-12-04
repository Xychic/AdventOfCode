use std::{fs, time::Instant};

#[derive(Debug, Clone)]
struct Board {
    nums: [[usize; 5]; 5],
    seen: Vec<usize>,
}

impl Board {
    fn new(nums: [[usize; 5]; 5]) -> Board {
        Board {
            nums,
            seen: Vec::with_capacity(25),
        }
    }

    fn is_finished(&self) -> bool {
        // Check rows
        for row in self.nums {
            if row.iter().all(|n| self.seen.contains(n)) {
                return true;
            }
        }
        // Check cols
        for i in 0..self.nums[0].len() {
            if (0..self.nums.len()).all(|j| self.seen.contains(&self.nums[j][i])) {
                return true;
            }
        }
        false
    }

    fn get_score(&self) -> usize {
        self.seen[self.seen.len() - 1]
            * self
                .nums
                .iter()
                .map(|row| row.iter().filter(|n| !self.seen.contains(n)).sum::<usize>())
                .sum::<usize>()
    }
}

type Input = (Vec<usize>, Vec<Board>);

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");

    let start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1(&raw_input),
        Instant::now() - start
    );
    let start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2(&raw_input),
        Instant::now() - start
    );
}

fn parse(input: &str) -> Input {
    let parts: Vec<_> = input.trim().split("\n\n").collect();
    let nums = parts[0].split(',').map(|x| x.parse().unwrap()).collect();
    let mut boards = Vec::with_capacity(parts.len() - 1);

    for board in parts.iter().skip(1) {
        let mut board_vec = Vec::with_capacity(5);
        for row in board.trim().split('\n') {
            let nums: Vec<usize> = row
                .split(' ')
                .filter(|a| !a.is_empty())
                .map(|x| x.parse().unwrap())
                .collect();
            board_vec.push([nums[0], nums[1], nums[2], nums[3], nums[4]]);
        }
        boards.push(Board::new([
            board_vec[0],
            board_vec[1],
            board_vec[2],
            board_vec[3],
            board_vec[4],
        ]))
    }
    (nums, boards)
}

fn part_1(input: &str) -> usize {
    let (nums, mut boards) = parse(input);
    for n in nums {
        for b in boards.iter_mut() {
            b.seen.push(n);
            if b.is_finished() {
                return b.get_score();
            }
        }
    }

    unreachable!()
}

fn part_2(input: &str) -> usize {
    let (nums, mut boards) = parse(input);
    for n in nums {
        for b in boards.iter_mut() {
            b.seen.push(n);
        }
        if boards.len() == 1 && boards[0].is_finished() {
            return boards[0].get_score();
        }

        boards.retain(|b| !b.is_finished());
    }
    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1() {
        let test_input = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7";
        let test_answer = 4512;
        assert_eq!(part_1(&test_input), test_answer);
    }

    #[test]
    fn test_part_2() {
        let test_input = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7";
        let test_answer = 1924;
        assert_eq!(part_2(&test_input), test_answer);
    }
}
