use std::collections::HashMap;

type Input<'a> = HashMap<String, usize>;

pub fn parse(input: &str) -> Input {
    let mut path = Vec::new();
    let mut parsed = HashMap::new();

    for line in input.trim().lines() {
        if line.starts_with("$") {
            if line.starts_with("$ ls") {
                continue;
            }
            match line[2..].split_once(' ').unwrap() {
                ("cd", "..") => {
                    path.pop();
                }
                ("cd", name) => {
                    path.push(name);
                }
                _ => unreachable!(),
            }
        } else {
            match line.split_once(' ').unwrap() {
                ("dir", _) => continue,
                (size, _) => {
                    let size: usize = size.parse().unwrap();
                    for dir_len in 1..=path.len() {
                        let to_add = &path[..dir_len].join("/");
                        if let Some(val) = parsed.get_mut(to_add) {
                            *val += size;
                        } else {
                            parsed.insert(to_add.to_owned(), size);
                        }
                    }
                }
            }
        }
    }
    parsed
}

pub fn part_1(input: &Input) -> usize {
    input.values().filter(|&&size| size <= 100000).sum()
}

pub fn part_2(input: &Input) -> usize {
    let total_size = 70000000;
    let free = total_size - *input.get("/").unwrap();
    let need_to_free = 30000000 - free;
    *input
        .values()
        .filter(|&&size| size >= need_to_free)
        .min()
        .unwrap()
}
