// » cargo run --release
//    Compiling pangram v0.0.0 (/Users/dnn/Devel/Exercism/rust/pangram)
//     Finished release [optimized] target(s) in 0.33s
//      Running `target/release/pangram`
// bitset   11:       110ns (R²=1.000, 9131584 iterations in 143 samples)
// hashset  11:       534ns (R²=0.999, 1987290 iterations in 127 samples)
// bitset   43:       183ns (R²=1.000, 5669992 iterations in 138 samples)
// hashset  43:   1us 473ns (R²=1.000, 696525 iterations in 116 samples)
// bitset   59:       215ns (R²=1.000, 4685942 iterations in 136 samples)
// hashset  59:   1us 658ns (R²=1.000, 633203 iterations in 115 samples)
// bitset   67:       208ns (R²=1.000, 5154537 iterations in 137 samples)
// hashset  67:   1us 655ns (R²=1.000, 633203 iterations in 115 samples)
// bitset  445:       882ns (R²=1.000, 1233946 iterations in 122 samples)
// hashset 445:    6us 37ns (R²=1.000, 166733 iterations in 101 samples)

use bit_set::BitSet;
use std::collections::HashSet;

/// Determine whether a sentence is a pangram with a bitset
pub fn is_pangram(sentence: &str) -> bool {
    let s = sentence
        .chars()
        .filter(|c| c.is_ascii_alphabetic())
        .map(|c| c.to_ascii_lowercase() as usize - 'a' as usize)
        .collect::<BitSet>();

    s.len() == 26
}

/// Determine whether a sentence is pangram with a jhashset
pub fn is_pangram_hs(sentence: &str) -> bool {
    let s = sentence
        .chars()
        .filter(|c| c.is_ascii_alphabetic())
        .map(|c| c.to_ascii_lowercase())
        .collect::<HashSet<_>>();

    s.len() == 26
}
