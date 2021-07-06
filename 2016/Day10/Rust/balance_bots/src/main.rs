use std::fs;
use std::collections::HashMap;

fn main() {
    let input: String = fs::read_to_string("../../../input.txt").expect("error reading file");
    let lines: Vec<&str> = input.trim().split("\n").collect();

    let bots: Vec<(String, String, String)> = 
    lines
    .clone()
    .iter()
    .filter(|line| {
        ! line.contains("value")
    })
    .map(|line| {
        let data = line
        .split(" ")
        .collect::<Vec<&str>>();
        (
            format!("bot{}", data[1]), 
            format!("{}{}", data[5], data[6]),
            format!("{}{}", data[10], data[11])
        ) 
    })
    .collect();

    let values: Vec<(usize, usize)> = 
    lines
    .clone()
    .iter()
    .filter(|line| {
        line.contains("value")
    })
    .map(|line| {
        let data = 
        line
        .split(" ")
        .collect::<Vec<&str>>();
        (
            data[1].parse::<usize>().unwrap(), 
            data[5].parse::<usize>().unwrap()
        )
        
    })
    .collect();

    println!("Part 1: {}", part_1(bots.clone(), values.clone()));
    println!("Part 2: {}", part_2(bots.clone(), values.clone()));
}

fn part_1(bots: Vec<(String, String, String)>, values: Vec<(usize, usize)>) -> usize {
    let mut data: HashMap<String, usize> = HashMap::new();
    let mut ins: HashMap<String, (String, String)> = HashMap::new();
    let mut ans: Vec<String> = Vec::new();    

    for (bot, low, high) in bots {
        data.insert(bot.clone(), 0);
        ins.insert(bot, (low.clone(), high.clone()));
        if low.contains("output") { data.insert(low, 0); }
        if high.contains("output") { data.insert(high, 0); }
    }

    for (val, bot) in values {
        ans.extend(insert(format!("bot{}", bot), val, &mut data, &ins));
    }

    ans[0][3..].parse().unwrap()
}

fn part_2(bots: Vec<(String, String, String)>, values: Vec<(usize, usize)>) -> usize {
    let mut data: HashMap<String, usize> = HashMap::new();
    let mut ins: HashMap<String, (String, String)> = HashMap::new();
    for (bot, low, high) in bots {
        data.insert(bot.clone(), 0);
        ins.insert(bot, (low.clone(), high.clone()));
        if low.contains("output") { data.insert(low, 0); }
        if high.contains("output") { data.insert(high, 0); }
    }

    for (val, bot) in values {
        insert(format!("bot{}", bot), val, &mut data, &ins);
    }

    (0..3)
    .map(|i| {
        data[&format!("output{}", i)]
    })
    .fold(1usize, |a, b| a * b)
}

fn insert(container: String, val: usize, data: &mut HashMap<String, usize>, ins: &HashMap<String, (String, String)>) -> Vec<String> {
    let mut ans: Vec<String> = Vec::new();

    if data[&container] == 0 {
        data.insert(container, val);
    } else {
        let min = *[data[&container], val].iter().min().unwrap();
        let max = *[data[&container], val].iter().max().unwrap();

        if min == 17 && max == 61 { ans.push(container.clone()); }
        
        let (low, high) = &ins[&container];
        data.insert(container, 0);
        ans.extend(insert(low.to_string(), min, data, &ins));
        ans.extend(insert(high.to_string(), max, data, &ins));
    }
    ans
}
