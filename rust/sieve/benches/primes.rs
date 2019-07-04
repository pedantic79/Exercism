use criterion::{criterion_group, criterion_main, Criterion, ParameterizedBenchmark};
use sieve::*;

fn bench(c: &mut Criterion) {
    c.bench(
        "Prime Sieve",
        ParameterizedBenchmark::new(
            "vec",
            |b, i| b.iter(|| primes_up_to(*i)),
            vec![250, 500, 1000, 2000, 4000],
        )
        .with_function("btm", |b, i| b.iter(|| primes_up_to_btm(*i)))
        .with_function("hm", |b, i| b.iter(|| primes_up_to_hm(*i))),
    );
}

criterion_group!(benches, bench);
criterion_main!(benches);
