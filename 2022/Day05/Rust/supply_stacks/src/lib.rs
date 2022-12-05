type Input<'a> = (Vec<Vec<char>>, Vec<(usize, usize, usize)>);

pub fn parse(input: &str) -> Input {
    let (puzzle_str, moves_str) = input.split_once("\n\n").unwrap();
    let mut puzzle_lines = puzzle_str.lines();
    let mut towers = vec![Vec::new(); (puzzle_lines.next_back().unwrap().len() + 1) / 4];

    for line in puzzle_lines.rev() {
        for (index, chr) in line.char_indices() {
            if chr.is_alphabetic() {
                let pos = (index - 1) / 4;
                towers[pos].push(chr);
            }
        }
    }

    let moves = moves_str
        .lines()
        .map(|line| {
            let parts: Vec<_> = line.split(' ').collect();
            (
                parts[1].parse().unwrap(),
                parts[3].parse::<usize>().unwrap() - 1,
                parts[5].parse::<usize>().unwrap() - 1,
            )
        })
        .collect();

    (towers, moves)
}

pub fn part_1((towers, moves): &Input) -> String {
    let mut towers = towers.to_owned();
    for &(count, start, end) in moves {
        for _ in 0..count {
            let val = towers[start].pop().unwrap();
            towers[end].push(val);
        }
    }
    towers
        .iter()
        .filter(|t| !t.is_empty())
        .map(|t| t.last().unwrap())
        .collect()
}

pub fn part_2((towers, moves): &Input) -> String {
    let mut towers = towers.to_owned();
    for &(count, start, end) in moves {
        for &val in (0..count)
            .map(|_| towers[start].pop().unwrap())
            .collect::<Vec<_>>()
            .iter()
            .rev()
        {
            towers[end].push(val);
        }
    }
    towers
        .iter()
        .filter(|t| !t.is_empty())
        .map(|t| t.last().unwrap())
        .collect()
}
