use criterion::{criterion_group, criterion_main, Criterion};
use sea_cucumber::{parse, part_1};
use std::fs;

pub fn criterion_benchmark(c: &mut Criterion) {
    let raw_input = fs::read_to_string("../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    c.bench_function("2021::Day25::parse", |b| b.iter(|| parse(&raw_input)));
    c.bench_function("2021::Day25::part 1", |b| b.iter(|| part_1(&input)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
