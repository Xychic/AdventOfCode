use std::{
    collections::{HashMap, HashSet},
    fs,
    hash::Hash,
    ops::{Add, Sub},
    time::Instant,
};

use itertools::Itertools;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Point {
    x: isize,
    y: isize,
    z: isize,
}

impl Point {
    fn new(x: isize, y: isize, z: isize) -> Point {
        Point { x, y, z }
    }

    fn get_coords(&self) -> [isize; 3] {
        [self.x, self.y, self.z]
    }

    fn manhattan_distance(&self, other: &Point) -> usize {
        (other - self)
            .get_coords()
            .iter()
            .map(|p| p.abs() as usize)
            .sum()
    }
}

impl Sub for Point {
    type Output = Point;

    fn sub(self, rhs: Self) -> Self::Output {
        Point::new(self.x - rhs.x, self.y - rhs.y, self.z - rhs.z)
    }
}

impl Sub for &Point {
    type Output = Point;

    fn sub(self, rhs: Self) -> Self::Output {
        Point::new(self.x - rhs.x, self.y - rhs.y, self.z - rhs.z)
    }
}

impl Add for &Point {
    type Output = Point;

    fn add(self, rhs: Self) -> Self::Output {
        Point::new(self.x + rhs.x, self.y + rhs.y, self.z + rhs.z)
    }
}

#[derive(Debug)]
struct Scanner {
    id: usize,
    beacons: Vec<Point>,
}

impl Scanner {
    fn new(scanner: &str) -> Scanner {
        let mut lines = scanner.lines();
        let id = lines
            .next()
            .unwrap()
            .split(' ')
            .nth(2)
            .unwrap()
            .parse()
            .unwrap();
        let beacons = lines
            .map(|l| {
                let mut parts = l.split(',');
                Point::new(
                    parts.next().unwrap().parse().unwrap(),
                    parts.next().unwrap().parse().unwrap(),
                    parts.next().unwrap().parse().unwrap(),
                )
            })
            .collect();
        Scanner { id, beacons }
    }

    fn get_rotations(&self) -> Vec<Vec<Point>> {
        let perms: Vec<_> = [0, 1, 2].iter().permutations(3).collect();
        (0..48)
            .map(|d| {
                let p = &perms[d / 8];
                self.beacons
                    .iter()
                    .map(|b| {
                        let points = b.get_coords();
                        Point::new(
                            points[*p[0]] * if d % 2 == 1 { -1 } else { 1 },
                            points[*p[1]] * if (d / 2) % 2 == 1 { -1 } else { 1 },
                            points[*p[2]] * if (d / 4) % 2 == 1 { -1 } else { 1 },
                        )
                    })
                    .collect()
            })
            .collect()
    }
}

type Input = Vec<Scanner>;

fn main() {
    let raw_input = fs::read_to_string("../../../input.txt").expect("error reading file");
    let input = parse(&raw_input);

    let start = Instant::now();
    println!(
        "Part 1: {}, took {:?}",
        part_1(&input),
        Instant::now() - start
    );
    let start = Instant::now();
    println!(
        "Part 2: {}, took {:?}",
        part_2(&input),
        Instant::now() - start
    );
}

fn parse(input: &str) -> Input {
    input
        .trim()
        .split("\n\n")
        .map(|s| Scanner::new(s))
        .collect()
}

fn part_1(input: &[Scanner]) -> usize {
    let mut rotations = HashMap::with_capacity(input.len() * 48);
    for (index, scanner) in input.iter().enumerate() {
        for (rot, points) in scanner.get_rotations().iter().enumerate() {
            rotations.insert((index, rot), points.clone());
        }
    }

    let mut not_found = HashSet::with_capacity(input.len());
    let mut found = HashSet::with_capacity(input.len() * input[0].beacons.len());
    for i in 0..input.len() {
        not_found.insert(i);
    }
    for b in &input[0].beacons {
        found.insert(b.clone());
    }

    while !not_found.is_empty() {
        let mut correct = None;
        'beacons: for &beacon_index in &not_found {
            for rot_index in 0..48 {
                let scanner_rot = rotations.get(&(beacon_index, rot_index)).unwrap();
                let mut seen = HashMap::with_capacity(48 * found.len());
                for beacon in scanner_rot {
                    for f in &found {
                        *seen.entry(f - beacon).or_insert(0) += 1;
                    }
                }
                for (p, count) in seen {
                    if count >= 12 {
                        for scanner_point in scanner_rot {
                            found.insert(scanner_point + &p);
                        }
                        correct = Some(beacon_index);
                    }
                }
                if correct.is_some() {
                    break 'beacons;
                }
            }
        }
        if let Some(c) = correct {
            not_found.remove(&c);
        }
    }
    found.len()
}

fn part_2(input: &[Scanner]) -> usize {
    let mut rotations = HashMap::with_capacity(input.len() * 48);
    for (index, scanner) in input.iter().enumerate() {
        for (rot, points) in scanner.get_rotations().iter().enumerate() {
            rotations.insert((index, rot), points.clone());
        }
    }

    let mut not_found = HashSet::with_capacity(input.len());
    let mut found = HashSet::with_capacity(input.len() * input[0].beacons.len());
    for i in 0..input.len() {
        not_found.insert(i);
    }
    for b in &input[0].beacons {
        found.insert(b.clone());
    }

    let mut scanners = HashSet::with_capacity(input.len());

    while !not_found.is_empty() {
        let mut correct = None;
        'beacons: for &beacon_index in &not_found {
            for rot_index in 0..48 {
                let scanner_rot = rotations.get(&(beacon_index, rot_index)).unwrap();
                let mut seen = HashMap::with_capacity(48 * found.len());
                for beacon in scanner_rot {
                    for f in &found {
                        *seen.entry(f - beacon).or_insert(0) += 1;
                    }
                }
                for (p, count) in seen {
                    if count >= 12 {
                        scanners.insert(p.clone());
                        for scanner_point in scanner_rot {
                            found.insert(scanner_point + &p);
                        }
                        correct = Some(beacon_index);
                    }
                }
                if correct.is_some() {
                    break 'beacons;
                }
            }
        }
        if let Some(c) = correct {
            not_found.remove(&c);
        }
    }

    let mut ans = 0;
    for a in &scanners {
        for b in scanners.iter().skip(1) {
            ans = ans.max(a.manhattan_distance(b));
        }
    }
    ans
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST_INPUT_1: &str = "
--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14";
    const TEST_INPUT_2: &str = TEST_INPUT_1;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse(&TEST_INPUT_1)), 79);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse(&TEST_INPUT_2)), 3621);
    }
}
