use criterion::{criterion_group, criterion_main, Criterion};
use snowverload::{parse, part_1};
use std::fs;

/// Benchmark for part 1 of 2023 Day 25 (`snowverload`)
///
/// # Panics
pub fn criterion_benchmark(c: &mut Criterion) {
    let raw_input = fs::read_to_string("../../input.txt").expect("error reading file");
    let input = parse(&raw_input);
    let mut paths: Vec<_> = fs::read_dir("./benches/graphs")
        .unwrap()
        .filter_map(|x| match x {
            Ok(x) => Some(x.path().display().to_string()),
            Err(_) => None,
        })
        .collect();
    paths.sort_by_key(|p| {
        p.split_once('-')
            .unwrap()
            .1
            .split_once('.')
            .unwrap()
            .0
            .parse::<usize>()
            .unwrap()
    });
    // dbg!(paths);
    for path in paths {
        let id = path.split('/').last().unwrap().split_once('.').unwrap().0;
        let raw_input = fs::read_to_string(&path).unwrap();
        let input = parse(&raw_input);
        c.bench_function(&format!("2023::Day25::{id}"), |b| b.iter(|| part_1(&input)));
        println!("{id}: {}", part_1(&input));
    }
    c.bench_function("2023::Day25::parse", |b| b.iter(|| parse(&raw_input)));
    c.bench_function("2023::Day25::part 1", |b| b.iter(|| part_1(&input)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
