use criterion::{criterion_group, criterion_main, Criterion};
use regolith_reservoir::*;
use std::fs;

pub fn criterion_benchmark(c: &mut Criterion) {
    let raw_input = fs::read_to_string("../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    c.bench_function("2022::Day14::parse", |b| b.iter(|| parse(&raw_input)));
    c.bench_function("2022::Day14::part 1", |b| b.iter(|| part_1(&input)));
    c.bench_function("2022::Day14::part 2", |b| b.iter(|| part_2(&input)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
