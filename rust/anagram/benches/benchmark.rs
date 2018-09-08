#[macro_use]
extern crate criterion;
extern crate anagram;

use anagram::*;
use criterion::Criterion;

fn criterion_benchmark(c: &mut Criterion) {
    let word = "allergy";

    let inputs = [
        "gallery",
        "ballerina",
        "regally",
        "clergy",
        "largely",
        "leading",
    ];

    c.bench_function("map allergy", move |b| {
        b.iter(|| anagrams_map(&word, &inputs))
    });
    c.bench_function("sort allergy", move |b| {
        b.iter(|| anagrams_sort(&word, &inputs))
    });
    c.bench_function("vec allergy", move |b| {
        b.iter(|| anagrams_vec(&word, &inputs))
    });
}

fn adeinr(c: &mut Criterion) {
    let word = "adeinr";
    let inputs = [
        "adrien", "adrine", "ainder", "anderi", "andire", "andrei", "andrie", "ardine", "dairen",
        "daneri", "danier", "darein", "darien", "darine", "dearin", "deiran", "denair", "denari",
        "denira", "derain", "derian", "derina", "dierna", "draine", "drenai", "drenia", "edrian",
        "erandi", "eridan", "erinda", "idanre", "indear", "indera", "naderi", "nadier", "nadire",
        "nareid", "neidar", "neriad", "nerida", "niedra", "radien", "raiden", "rained", "randie",
        "rdeina", "readin", "redian", "redina", "renida", "riande", "rienda",
    ];
    c.bench_function("map adeinr", move |b| {
        b.iter(|| anagrams_map(&word, &inputs))
    });
    c.bench_function("sort adeinr", move |b| {
        b.iter(|| anagrams_sort(&word, &inputs))
    });
    c.bench_function("vec adeinr", move |b| {
        b.iter(|| anagrams_vec(&word, &inputs))
    });
}

criterion_group!(benches, criterion_benchmark, adeinr);
criterion_main!(benches);
