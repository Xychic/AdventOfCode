use std::fs;


#[derive(Debug, Clone)]
struct Disc {
    number: usize,
    positions: usize,
    start_position: usize
}

type DataFormat = Vec<Disc>;

fn main() {
    let input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let discs = parse_input(&input.trim().split("\n").collect());

    println!("Part 1: {}", part_1(&discs));
    println!("Part 2: {}", part_2(&discs));
}

fn part_1(discs: &DataFormat) -> usize {
    for i in 0.. {
        if check_discs(&discs, i) {
            return i;
        }
    }
    0
}

fn part_2(discs: &DataFormat) -> usize {
    let size = discs.len()+1;
    let mut a = Vec::with_capacity(size);
    for d in discs {
        a.push(d.clone());
    }
    a.push(Disc { number: size, positions: 11, start_position: 0 });
    for i in 0.. {
        if check_discs(&a, i) {
            return i;
        }
    }
    0    
}

fn parse_input(lines: &Vec<&str>) -> DataFormat {
    lines
    .iter()
    .map(|l| {
        let data: Vec<String> = l
            .replace(".","")
            .replace(";", ",")
            .replace("=", " ")
            .replace(" #", " ")
            .replace(",", " ")
            .split(" ")
            .map(|x| {
                x.to_string()
            })
            .collect();
        let (number, positions, start_position) = (data[1].parse().unwrap(), data[3].parse().unwrap(), data[14].parse().unwrap());
        Disc{number, positions, start_position}
    })
    .collect()
}

fn check_discs(discs: &DataFormat, time: usize) -> bool {
    for d in discs {
        if (time + d.number + d.start_position) % d.positions != 0 {
            return false;
        }
    }
    true
}