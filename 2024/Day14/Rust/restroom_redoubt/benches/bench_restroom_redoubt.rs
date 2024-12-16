use criterion::{criterion_group, criterion_main, Criterion};
use restroom_redoubt::{parse, part_1};
use std::fs;

/// Benchmark for part 1 of 2024 Day 14 (`restroom_redoubt`)
///
/// # Panics
pub fn criterion_benchmark(c: &mut Criterion) {
    let raw_input = fs::read_to_string("../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    c.bench_function("2024::Day14::parse", |b| b.iter(|| parse(&raw_input)));
    c.bench_function("2024::Day14::part 1", |b| {
        b.iter(|| part_1::<101, 103>(&input));
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
