use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use palindrome_products::{
    palindrome_products_mutex, palindrome_products_reduce, palindrome_products_sequential,
};
use std::time::Duration;

const SIZES: [u64; 3] = [1999, 5999, 9999];

fn bench_method_reduce(c: &mut Criterion) {
    let mut group = c.benchmark_group("parallel");

    for size in SIZES.iter() {
        group.throughput(Throughput::Elements((size - 1000) as u64));
        group.sample_size(10);
        group.measurement_time(Duration::from_secs(60));

        group.bench_with_input(
            BenchmarkId::new("reduce", size.to_string()),
            &size,
            |b, &s| b.iter(|| palindrome_products_reduce(1000, *s)),
        );
    }

    group.finish();
}

fn bench_method_mutex(c: &mut Criterion) {
    let mut group = c.benchmark_group("parallel");

    for size in SIZES.iter() {
        group.throughput(Throughput::Elements((size - 1000) as u64));
        group.sample_size(10);
        group.measurement_time(Duration::from_secs(60));

        group.bench_with_input(
            BenchmarkId::new("mutex", size.to_string()),
            &size,
            |b, &s| b.iter(|| palindrome_products_mutex(1000, *s)),
        );
    }

    group.finish();
}

fn bench_method_sequential(c: &mut Criterion) {
    let mut group = c.benchmark_group("sequential");

    for (size, &duration) in SIZES.iter().zip([120, 180, 240].iter()) {
        group.throughput(Throughput::Elements((size - 1000) as u64));
        group.sample_size(10);
        group.measurement_time(Duration::from_secs(duration));

        group.bench_with_input(
            BenchmarkId::new("sequential", size.to_string()),
            &size,
            |b, &s| b.iter(|| palindrome_products_sequential(1000, *s)),
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_method_reduce,
    bench_method_mutex,
    bench_method_sequential
);
criterion_main!(benches);
