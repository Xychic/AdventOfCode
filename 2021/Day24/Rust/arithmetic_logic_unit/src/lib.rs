type Input<'a> = Vec<&'a str>;

pub fn parse(input: &str) -> Input {
    input.trim().split('\n').collect()
}

pub fn part_1(input: &Input) -> String {
    solve::<9>(input)
}

pub fn part_2(input: &Input) -> String {
    solve::<1>(input)
}

fn solve<const START: isize>(input: &Input) -> String {
    let mut data = [START; 14];
    let mut stack = Vec::with_capacity(14);

    for i in 0..14 {
        let [div, chk, add]: [isize; 3] = [4, 5, 15].map(|x| {
            input[x + (18 * i)]
                .split_ascii_whitespace()
                .last()
                .unwrap()
                .parse()
                .unwrap()
        });
        if div == 1 {
            stack.push((i, add));
        } else if div == 26 {
            let (j, add) = stack.pop().unwrap();
            data[i] = data[j] + add + chk;
            if data[i] > 9 {
                data[j] -= data[i] - 9;
                data[i] = 9;
            } else if data[i] < 1 {
                data[j] += 1 - data[i];
                data[i] = 1;
            }
        }
    }
    data.iter().map(std::string::ToString::to_string).collect()
}
