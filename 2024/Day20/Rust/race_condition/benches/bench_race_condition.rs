use criterion::{criterion_group, criterion_main, Criterion};
use race_condition::{parse, part_1, part_2};
use std::fs;

/// Benchmark for part 1 of 2024 Day 20 (`race_condition`)
///
/// # Panics
pub fn criterion_benchmark(c: &mut Criterion) {
    let raw_input = fs::read_to_string("../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    c.bench_function("2024::Day20::parse", |b| b.iter(|| parse(&raw_input)));
    c.bench_function("2024::Day20::part 1", |b| b.iter(|| part_1::<100>(&input)));
    c.bench_function("2024::Day20::part 2", |b| b.iter(|| part_2::<100>(&input)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
