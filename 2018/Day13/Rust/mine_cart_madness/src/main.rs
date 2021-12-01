use std::fs;

#[derive(Debug, Clone)]
struct Cart {
    pos: (usize, usize),
    dir: Direction,
    intersection: Intersection,
}

impl Cart {
    fn new(pos: (usize, usize), dir: Direction) -> Cart {
        Cart {
            pos,
            dir,
            intersection: Intersection::Left,
        }
    }

    fn step(&mut self, grid: &[Vec<char>]) {
        match self.dir {
            Direction::Up => {
                self.pos.1 -= 1;
                match grid[self.pos.1][self.pos.0] {
                    '/' => self.dir = Direction::Right,
                    '\\' => self.dir = Direction::Left,
                    '+' => {
                        self.dir = self.intersection.get_dir(&self.dir);
                        self.intersection = self.intersection.next()
                    }
                    _ => (),
                }
            }
            Direction::Down => {
                self.pos.1 += 1;
                match grid[self.pos.1][self.pos.0] {
                    '/' => self.dir = Direction::Left,
                    '\\' => self.dir = Direction::Right,
                    '+' => {
                        self.dir = self.intersection.get_dir(&self.dir);
                        self.intersection = self.intersection.next()
                    }
                    _ => (),
                }
            }
            Direction::Left => {
                self.pos.0 -= 1;
                match grid[self.pos.1][self.pos.0] {
                    '/' => self.dir = Direction::Down,
                    '\\' => self.dir = Direction::Up,
                    '+' => {
                        self.dir = self.intersection.get_dir(&self.dir);
                        self.intersection = self.intersection.next()
                    }
                    _ => (),
                }
            }
            Direction::Right => {
                self.pos.0 += 1;
                match grid[self.pos.1][self.pos.0] {
                    '/' => self.dir = Direction::Up,
                    '\\' => self.dir = Direction::Down,
                    '+' => {
                        self.dir = self.intersection.get_dir(&self.dir);
                        self.intersection = self.intersection.next()
                    }
                    _ => (),
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn left(&self) -> Direction {
        match self {
            Direction::Up => Direction::Left,
            Direction::Down => Direction::Right,
            Direction::Left => Direction::Down,
            Direction::Right => Direction::Up,
        }
    }

    fn right(&self) -> Direction {
        match self {
            Direction::Up => Direction::Right,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
            Direction::Right => Direction::Down,
        }
    }
}

#[derive(Debug, Clone)]
enum Intersection {
    Straight,
    Left,
    Right,
}

impl Intersection {
    fn next(&self) -> Intersection {
        match self {
            Intersection::Straight => Intersection::Right,
            Intersection::Left => Intersection::Straight,
            Intersection::Right => Intersection::Left,
        }
    }

    fn get_dir(&self, dir: &Direction) -> Direction {
        match self {
            Intersection::Straight => dir.clone(),
            Intersection::Left => dir.left(),
            Intersection::Right => dir.right(),
        }
    }
}

type Input = (Vec<Vec<char>>, Vec<Cart>);

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    println!("Part 1: {}", part_1(&raw_input));
    println!("Part 2: {}", part_2(&raw_input));
}

fn parse(input: &str) -> Input {
    let mut a: Vec<Vec<_>> = input.split('\n').map(|l| l.chars().collect()).collect();
    let mut carts = Vec::new();
    for (y, col) in a.iter_mut().enumerate() {
        for (x, item) in col.iter_mut().enumerate() {
            match item {
                '^' => {
                    carts.push(Cart::new((x, y), Direction::Up));
                    *item = '|';
                }
                'v' => {
                    carts.push(Cart::new((x, y), Direction::Down));
                    *item = '|';
                }
                '<' => {
                    carts.push(Cart::new((x, y), Direction::Left));
                    *item = '-';
                }
                '>' => {
                    carts.push(Cart::new((x, y), Direction::Right));
                    *item = '-';
                }
                _ => (),
            }
        }
    }
    (a, carts)
}

fn part_1(input: &str) -> String {
    let (grid, mut carts) = parse(input);
    loop {
        for i in 0..carts.len() {
            carts[i].step(&grid);
            for j in 0..carts.len() {
                if i != j && carts[i].pos == carts[j].pos {
                    return format!("{},{}", carts[i].pos.0, carts[i].pos.1);
                }
            }
        }
        carts.sort_by_key(|c| c.pos);
    }
}

fn part_2(input: &str) -> String {
    let (grid, mut carts) = parse(input);

    while carts.len() > 1 {
        let mut to_remove = Vec::with_capacity(carts.len());
        for i in 0..carts.len() {
            carts[i].step(&grid);
            for j in 0..carts.len() {
                if i != j && carts[i].pos == carts[j].pos {
                    to_remove.push(i);
                    to_remove.push(j);
                    break;
                }
            }
        }
        let mut index = 0;
        carts.retain(|_| {
            index += 1;
            !to_remove.contains(&(index - 1))
        });
        carts.sort_by_key(|c| c.pos);
    }
    return format!("{},{}", carts[0].pos.0, carts[0].pos.1);
}

fn _draw(grid: &[Vec<char>], carts: &[Cart]) {
    for (y, col) in grid.iter().enumerate() {
        for (x, c) in col.iter().enumerate() {
            let mut is_cart = false;
            for cart in carts {
                if cart.pos == (x, y) {
                    is_cart = true;
                    print!(
                        "{}",
                        match cart.dir {
                            Direction::Up => '^',
                            Direction::Down => 'v',
                            Direction::Left => '<',
                            Direction::Right => '>',
                        }
                    );
                }
            }
            if !is_cart {
                print!("{}", c);
            }
        }
        println!();
    }
}
