use beacon_exclusion_zone::*;
use criterion::{criterion_group, criterion_main, Criterion};
use std::fs;

pub fn criterion_benchmark(c: &mut Criterion) {
    let raw_input = fs::read_to_string("../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    c.bench_function("2022::Day15::parse", |b| b.iter(|| parse(&raw_input)));
    c.bench_function("2022::Day15::part 1", |b| {
        b.iter(|| part_1(&input, 2000000))
    });
    c.bench_function("2022::Day15::part 2", |b| {
        b.iter(|| part_2(&input, 4000000))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
