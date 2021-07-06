use std::fs;

#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd)]
struct Room {
    name: String,
    sector: usize,
    checksum: String,
}

fn main() {
    let input: String = fs::read_to_string("../../../input.txt").expect("error reading file");
    let lines: Vec<&str> = input.trim().split("\n").collect();
    let rooms: Vec<Room> = parse_input(lines);

    println!("Part 1: {}", part_1(rooms.clone()));
    println!("Part 2: {}", part_2(rooms.clone()));
}

fn part_1(rooms: Vec<Room>) -> usize {
    let mut ans = 0;

    for room in rooms {
        let mut sorted = room.name.replace("-","").chars().collect::<Vec<char>>();
        sorted.sort();
        sorted.dedup();
        sorted.sort_by(|a, b| {room.name.matches(*b).count().cmp(&room.name.matches(*a).count())});
        let sorted = sorted[0..5].iter().collect::<String>();
        if sorted == room.checksum { ans += room.sector; }
    }

    ans
}

fn part_2(rooms: Vec<Room>) -> usize {
    for room in rooms {
        let mut sorted = room.name.replace("-","").chars().collect::<Vec<char>>();
        sorted.sort();
        sorted.dedup();
        sorted.sort_by(|a, b| {room.name.matches(*b).count().cmp(&room.name.matches(*a).count())});
        let sorted = sorted[0..5].iter().collect::<String>();
        
        if sorted != room.checksum {continue;}

        let decoded = 
            room
            .name
            .chars()
            .map(|c| {
                if c == '-' {' '} 
                else {
                    char::from_u32(((c as u32 - 'a' as u32 + room.sector as u32) % 26) + 'a' as u32)
                    .unwrap()
                }
            })
            .collect::<String>();
        if decoded == "northpole object storage" { return room.sector; }
    }

    0
}

fn parse_input(lines: Vec<&str>) -> Vec<Room> {
    let a = lines
    .iter()
    .map(|line| {
        Room {
            name: 
                line[0..line.len()-11]
                .to_string(),
            sector: 
                line[line.len()-10..line.len()-7]
                .to_string()
                .parse::<usize>()
                .expect("Nan"),
            checksum: 
                line[line.len()-6..line.len()-1]
                .to_string()
        }
    })
    .collect::<Vec<Room>>();
    
    a
}
