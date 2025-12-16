use christmas_tree_farm::{parse, part_1};
use criterion::{criterion_group, criterion_main, Criterion};
use std::fs;

/// Benchmark for part 1 of 2025 Day 12 (`christmas_tree_farm`)
///
/// # Panics
pub fn criterion_benchmark(c: &mut Criterion) {
    let raw_input = fs::read_to_string("../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    c.bench_function("2025::Day12::parse", |b| b.iter(|| parse(&raw_input)));
    c.bench_function("2025::Day12::part 1", |b| b.iter(|| part_1(&input)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
