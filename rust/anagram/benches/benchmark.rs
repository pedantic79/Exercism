use anagram::*;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::collections::{BTreeMap, HashSet};

type BoxedAnagrams = Box<dyn for<'a> Fn(&str, &[&'a str]) -> HashSet<&'a str>>;

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
            if lower.len() != anagram.len() {
                return false;
            }

            let lc_anagram = anagram.to_lowercase();

            if lower == lc_anagram {
                return false;
            }

            let mut word_freq = freq.clone();
            for c in lc_anagram.chars() {
                match word_freq.get_mut(&c) {
                    Some(0) => return false,
                    Some(v) => *v -= 1,
                    None => return false,
                }
            }

            true
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

    common(c, word, &inputs, "allergy");
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

    common(c, word, &inputs, "all_match");
}

fn no_matches(c: &mut Criterion) {
    let word = "abcdef";
    let inputs = [
        "abcdeg", "abcdeh", "abcdei", "abcdej", "abcdek", "abcdel", "abcdem", "abcden", "abcdeo",
        "abcdep", "abcdeq", "abcder", "abcdes", "abcdet", "abcdeu", "abcdev", "abcdew", "abcdex",
        "abcdey", "abcdez", "abcdea", "abcdeb", "abcdec", "abcded", "abcdfa", "abcdfb", "abcdfc",
        "abcdfd", "abcdfh", "abcdfi", "abcdfj", "abcdfk", "abcdfl", "abcdfm", "abcdfn", "abcdfo",
        "abcdfp", "abcdfq", "abcdfr", "abcdfs", "abcdft", "abcdfu", "abcdfv", "abcdfw", "abcdfx",
        "abcdfy", "abcdfz", "abcdgh", "abcdgi", "abcdgj", "abcdgk", "abcdgl",
    ];

    common(c, word, &inputs, "no_matches");
}

fn common(c: &mut Criterion, word: &str, inputs: &[&str], group_name: &str) {
    let mut group = c.benchmark_group(group_name);

    let funcs: [(&str, BoxedAnagrams); 3] = [
        ("btmap", Box::new(anagrams_map)),
        ("vec", Box::new(anagrams_vec)),
        ("quick", Box::new(anagrams_quick)),
    ];
    let len = inputs.len() as u64;

    for (name, func) in funcs.iter() {
        group.throughput(Throughput::Elements(len));
        group.bench_with_input(
            BenchmarkId::new(*name, len.to_string()),
            &inputs,
            |b, &input_ptr| b.iter(|| func(word, &input_ptr)),
        );
    }
}

criterion_group!(benches, criterion_benchmark, adeinr, no_matches);
criterion_main!(benches);
