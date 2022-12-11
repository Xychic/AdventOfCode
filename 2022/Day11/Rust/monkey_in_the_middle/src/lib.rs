type Input<'a> = Vec<Monkey<'a>>;

#[derive(Clone)]
pub struct Monkey<'a> {
    id: usize,
    items: Vec<usize>,
    op: (&'a str, &'a str, &'a str),
    test: usize,
    if_true: usize,
    if_false: usize,
    inspected: usize,
}

impl<'a> Monkey<'a> {
    fn from_str(data: &'a str) -> Monkey<'a> {
        let mut parts = data.split('\n');
        let id = parts
            .next()
            .unwrap()
            .split_once(' ')
            .unwrap()
            .1
            .strip_suffix(':')
            .unwrap()
            .parse()
            .unwrap();
        let items = parts
            .next()
            .unwrap()
            .split_once(": ")
            .unwrap()
            .1
            .split(", ")
            .map(|x| x.parse().unwrap())
            .collect();
        let op_parts: Vec<_> = parts
            .next()
            .unwrap()
            .split_once("= ")
            .unwrap()
            .1
            .splitn(3, ' ')
            .collect();
        let op = (op_parts[0], op_parts[1], op_parts[2]);
        let test = parts
            .next()
            .unwrap()
            .split_once("Test: divisible by ")
            .unwrap()
            .1
            .parse()
            .unwrap();
        let if_true = parts
            .next()
            .unwrap()
            .split_once("throw to monkey ")
            .unwrap()
            .1
            .parse()
            .unwrap();
        let if_false = parts
            .next()
            .unwrap()
            .split_once("throw to monkey ")
            .unwrap()
            .1
            .parse()
            .unwrap();
        Monkey {
            id,
            items,
            op,
            test,
            if_true,
            if_false,
            inspected: 0,
        }
    }

    fn inspect(&mut self, item: usize) -> usize {
        self.inspected += 1;

        let b = match self.op.2 {
            "old" => item,
            x => x.parse().unwrap(),
        };
        match self.op.1 {
            "*" => item * b,
            _ => item + b,
        }
    }

    fn get_and_clear(&mut self) -> Vec<usize> {
        let cloned = self.items.clone();
        self.items.clear();
        cloned
    }
}

pub fn parse(input: &str) -> Input {
    input.trim().split("\n\n").map(Monkey::from_str).collect()
}

pub fn part_1(input: &Input) -> usize {
    let mut monkeys = input.to_owned();
    for _ in 0..20 {
        for i in 0..monkeys.len() {
            assert_eq!(i, monkeys[i].id);
            for item in monkeys[i].get_and_clear() {
                let mut to_test = monkeys[i].inspect(item);
                to_test /= 3;
                let index = if to_test % monkeys[i].test == 0 {
                    monkeys[i].if_true
                } else {
                    monkeys[i].if_false
                };
                monkeys[index].items.push(to_test);
            }
        }
    }

    monkeys.sort_by(|a, b| b.inspected.cmp(&a.inspected));
    monkeys.iter().take(2).map(|m| m.inspected).product()
}

pub fn part_2(input: &Input) -> usize {
    let mut monkeys = input.to_owned();
    let modulo: usize = monkeys.iter().map(|m| m.test).product();
    for _ in 0..10_000 {
        for i in 0..monkeys.len() {
            for item in monkeys[i].get_and_clear() {
                let to_test = monkeys[i].inspect(item) % modulo;
                let index = if to_test % monkeys[i].test == 0 {
                    monkeys[i].if_true
                } else {
                    monkeys[i].if_false
                };

                monkeys[index].items.push(to_test);
            }
        }
    }

    monkeys.sort_by(|a, b| b.inspected.cmp(&a.inspected));
    monkeys.iter().take(2).map(|m| m.inspected).product()
}
