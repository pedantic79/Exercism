use anagram::*;
use criterion::{criterion_group, criterion_main, Criterion};
use std::collections::{BTreeMap, HashSet};

fn build_map_freq(s: &str) -> BTreeMap<char, i32> {
    s.chars().fold(BTreeMap::new(), |mut hm, c| {
        *hm.entry(c).or_insert(0) += 1;
        hm
    })
}

fn anagrams_quick<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let lower = word.to_lowercase();
    let freq = build_map_freq(&lower);

    possible_anagrams
        .iter()
        .filter(|anagram| {
            let lc_anagram = anagram.to_lowercase();
            if lower == lc_anagram || lower.len() != lc_anagram.len() {
                return false;
            }

            let mut word_freq = freq.clone();
            for c in lc_anagram.chars() {
                if let Some(v) = word_freq.get_mut(&c) {
                    if *v > 0 {
                        *v -= 1;
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }

            word_freq.values().all(|&x| x == 0)
        })
        .copied()
        .collect()
}

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
        b.iter(|| anagrams_map(word, &inputs))
    });
    c.bench_function("quick allergy", move |b| {
        b.iter(|| anagrams_quick(word, &inputs))
    });
    c.bench_function("vec allergy", move |b| {
        b.iter(|| anagrams_vec(word, &inputs))
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

    c.bench_function("map all-match", move |b| {
        b.iter(|| anagrams_map(word, &inputs))
    });
    c.bench_function("quick all-match", move |b| {
        b.iter(|| anagrams_quick(word, &inputs))
    });
    c.bench_function("vec all-match", move |b| {
        b.iter(|| anagrams_vec(word, &inputs))
    });
}

criterion_group!(benches, criterion_benchmark, adeinr);
criterion_main!(benches);
