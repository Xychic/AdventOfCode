use std::fs;
use std::io::{self, Write};

const FANCY_PRINT: bool = true; // Takes much longer but looks cooler

fn main() {
    let mut input: String = fs::read_to_string("../../../input.txt").expect("error reading file");
    input = input.trim().to_string();

    println!("Part 1: {}", part_1(input.clone()));
    println!("Part 2: {}", part_2(input.clone()));
}

fn part_1(input: String) -> String {
    let mut chars_found = 0;
    let mut ans = String::with_capacity(8);

    for i in 0..usize::MAX {
        let mut test = input.clone();
        test.push_str(&i.to_string());

        let result = md5::compute(test.as_bytes());
        let hash: String = format!("{:x}", result);
        if FANCY_PRINT {
            print!("\rPart 1: {}{}", ans, hash[5+chars_found..13].to_string());
        }

        if result[..2] == [0, 0] && result[2] <= 0x0F {
            let c: char = hash.chars().nth(5).unwrap();
            io::stdout().flush().unwrap();
            ans.push(c);
            chars_found += 1;
            if chars_found == 8 {break;}
        }
    }
    print!("\r");

    ans
}

fn part_2(input: String) -> String {
    let mut ans: [char; 8] = ['.'; 8];

    for i in 0..usize::MAX {
        let mut test = input.clone();
        test.push_str(&i.to_string());

        let result = md5::compute(test.as_bytes());
        let hash: String = format!("{:x}", result);

        if FANCY_PRINT {
            let mut to_print: String = String::with_capacity(8);
            for (i, c) in ans.iter().enumerate() {
                if c == &'.' {to_print.push(hash.chars().nth(6+i).unwrap())}
                else {to_print.push(*c);}
            }
            print!("\rPart 2: {}", to_print);
        }
        
        if result[..2] == [0, 0] && result[2] <= 0x0F {

            let pos: char = hash.chars().nth(5).unwrap();
            if ! pos.is_digit(10) {continue;}

            let pos: usize = (pos as u32 - '0' as u32) as usize;
            if pos > 7 {continue;}
            if ans[pos] != '.' {continue;}

            let c: char = hash.chars().nth(6).unwrap();

            ans[pos] = c;

            if ! ans.contains(&'.') {break;}
        }
    }
    print!("\r");
    ans.iter().collect::<String>()
}
