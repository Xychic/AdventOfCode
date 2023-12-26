use criterion::{criterion_group, criterion_main, Criterion};
use std::fs;
use step_counter::{parse, part_1, part_2};

/// Benchmark for part 1 of 2023 Day 21 (`step_counter`)
///
/// # Panics
pub fn criterion_benchmark(c: &mut Criterion) {
    let raw_input = fs::read_to_string("../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    c.bench_function("2023::Day21::parse", |b| b.iter(|| parse(&raw_input)));
    c.bench_function("2023::Day21::part 1", |b| b.iter(|| part_1(&input, 16)));
    c.bench_function("2023::Day21::part 2", |b| b.iter(|| part_2(&input, 16)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
