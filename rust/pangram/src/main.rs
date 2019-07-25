use easybench::bench;
use pangram::*;

const INPUT: [&str; 5] = [
    "hello world",
    "the quick brown fox jumps over the lazy dog",
    "a quick movement of the enemy will jeopardize five gunboats",
    "Victor jagt zwölf Boxkämpfer quer über den großen Sylter Deich.",
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
];

macro_rules! bench_pangram {
    ($input:expr) => {
        println!(
            "{:<8}{:>3}: {}",
            "bitset",
            $input.len(),
            bench(|| is_pangram($input)),
        );
        println!(
            "{:<8}{:>3}: {}",
            "hashset",
            $input.len(),
            bench(|| is_pangram_hs($input)),
        );
    };
}

fn main() {
    for sentence in INPUT.iter() {
        bench_pangram!(sentence);
    }
}
