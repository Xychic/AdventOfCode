use f128::f128;

type Vector = (f128, f128, f128);
type Point = Vector;
type Line = (Point, Vector);
type Input<'a> = Vec<Line>;

/// Parser for 2023 Day 24 (`never_tell_me_the_odds`)
///
/// # Panics
#[must_use]
pub fn parse(input: &str) -> Input {
    input
        .trim()
        .lines()
        .map(|l| {
            let (a, b) = l.split_once(" @ ").unwrap();
            let [px, py, pz] = a
                .split(", ")
                .map(|c| f128::from(c.parse::<isize>().unwrap()))
                .collect::<Vec<_>>()[..3]
            else {
                unreachable!()
            };
            let [vx, vy, vz] = b
                .split(", ")
                .map(|c| f128::from(c.trim().parse::<isize>().unwrap()))
                .collect::<Vec<_>>()[..3]
            else {
                unreachable!()
            };
            // ((xp, xv), (yp, yv), (zp, zv))
            ((px, py, pz), (vx, vy, vz))
        })
        .collect()
}

/// Solver for part 1 of 2023 Day 24 (`never_tell_me_the_odds`)
///
/// # Panics
#[must_use]
pub fn part_1<const A: u64, const B: u64>(input: &Input) -> usize {
    let min = f128::from(A);
    let max = f128::from(B);
    let mut ans = 0;
    for (i, &((px1, py1, _), (vx1, vy1, _))) in input.iter().enumerate() {
        for &((px2, py2, _), (vx2, vy2, _)) in input.iter().skip(i + 1) {
            let t1 =
                (((vx2 * (py1 - py2)) + (px2 * vy2)) - (px1 * vy2)) / ((vy2 * vx1) - (vx2 * vy1));
            let t2 =
                (((vx1 * (py2 - py1)) + (px1 * vy1)) - (px2 * vy1)) / ((vy1 * vx2) - (vx1 * vy2));

            let x = px1 + (t1 * vx1);
            let y = py1 + (t1 * vy1);

            if min <= x && x <= max && min <= y && y <= max && t1 >= f128::ZERO && t2 >= f128::ZERO
            {
                ans += 1;
            }
        }
    }
    ans
}

fn lin(
    ((v1_x, v1_y, v1_z), t1): &(Vector, f128),
    ((v2_x, v2_y, v2_z), t2): &(Vector, f128),
    ((v3_x, v3_y, v3_z), t3): &(Vector, f128),
) -> Vector {
    (
        t1 * v1_x + t2 * v2_x + t3 * v3_x,
        t1 * v1_y + t2 * v2_y + t3 * v3_y,
        t1 * v1_z + t2 * v2_z + t3 * v3_z,
    )
}

fn get_point_on_plane(
    ((p1_x, p1_y, p1_z), v1 @ (v1_x, v1_y, v1_z)): &Line,
    ((p2_x, p2_y, p2_z), v2 @ (v2_x, v2_y, v2_z)): &Line,
) -> (Point, f128) {
    let p2_to_p1 = (p1_x - p2_x, p1_y - p2_y, p1_z - p2_z);
    let v2_to_v1 = (v1_x - v2_x, v1_y - v2_y, v1_z - v2_z);
    let plane = cross(v1, v2);
    (cross(&p2_to_p1, &v2_to_v1), dot(&p2_to_p1, &plane))
}

fn cross(&(v1_x, v1_y, v1_z): &Vector, &(v2_x, v2_y, v2_z): &Vector) -> Vector {
    (
        v1_y * v2_z - v1_z * v2_y,
        v1_z * v2_x - v1_x * v2_z,
        v1_x * v2_y - v1_y * v2_x,
    )
}

fn dot(&(v1_x, v1_y, v1_z): &Vector, (v2_x, v2_y, v2_z): &Vector) -> f128 {
    v1_x * v2_x + v1_y * v2_y + v1_z * v2_z
}

fn not_parallel(v1: &Vector, v2: &Vector) -> bool {
    cross(v1, v2) != (f128::ZERO, f128::ZERO, f128::ZERO)
}

fn get_rock(
    l1 @ (p1, (v1_x, v1_y, v1_z)): &Line,
    l2 @ (p2, (v2_x, v2_y, v2_z)): &Line,
    l3: &Line,
) -> Vector {
    let (vec_a, at) = get_point_on_plane(l1, l2);
    let (vec_b, bt) = get_point_on_plane(l1, l3);
    let (vec_c, ct) = get_point_on_plane(l2, l3);

    let (p_vec_x, p_vec_y, p_vec_z) = lin(
        &(cross(&vec_b, &vec_c), at),
        &(cross(&vec_c, &vec_a), bt),
        &(cross(&vec_a, &vec_b), ct),
    );

    let t = dot(&vec_a, &cross(&vec_b, &vec_c));
    let (p_vec_x, p_vec_y, p_vec_z) = ((p_vec_x / t), (p_vec_y / t), (p_vec_z / t));

    let p_vec_to_v1 = (v1_x - p_vec_x, v1_y - p_vec_y, v1_z - p_vec_z);
    let p_vec_to_v2 = (v2_x - p_vec_x, v2_y - p_vec_y, v2_z - p_vec_z);
    let plane = cross(&p_vec_to_v1, &p_vec_to_v2);

    let (x, y, z) = lin(
        &(p_vec_to_v1, dot(&plane, &cross(p2, &p_vec_to_v2))),
        &(p_vec_to_v2, -dot(&plane, &cross(p1, &p_vec_to_v1))),
        &(plane, dot(p1, &plane)),
    );

    let t = dot(&plane, &plane);
    (x / t, y / t, z / t)
}

/// Solver for part 2 of 2023 Day 24 (`never_tell_me_the_odds`)
///
/// # Panics
#[must_use]
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
pub fn part_2(input: &Input) -> usize {
    // dbg!(input);
    let l1 = input[0];
    let (idx, l2) = input
        .iter()
        .enumerate()
        .skip(1)
        .find(|&(_, (_, v))| not_parallel(&l1.1, v))
        .unwrap();
    let l3 = input
        .iter()
        .skip(idx + 1)
        .find(|&(_, v)| not_parallel(&l1.1, v) && not_parallel(&l2.1, v))
        .unwrap();

    let (x, y, z) = get_rock(&l1, l2, l3);
    (x + y + z).into()
}
