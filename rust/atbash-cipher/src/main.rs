use atbash_cipher::*;
use easybench::bench;

macro_rules! benchmark {
    ($name:literal, $len:expr, $ben:ident, $input:ident) => {
        println!(
            "{:>8}/{:<4} {}",
            $name,
            $len,
            bench(|| $ben($input.chars()))
        );
    };
}

fn main() {
    for len in &[12, 48, 252, 1024] {
        let input = String::from_utf8(vec![b'X'; *len as usize]).unwrap();

        benchmark!("vec", len, split_vec, input);
        benchmark!("iterator", len, split_flat_map, input);
        benchmark!("fold_str", len, split_str_push, input);
        benchmark!("from_fn", len, split_from_fn, input);
    }
}

fn split_vec(iter: impl Iterator<Item = char>) -> String {
    let v = iter.collect::<Vec<char>>();
    v.chunks(5)
        .map(|s| s.iter().collect())
        .collect::<Vec<String>>()
        .join(" ")
}

fn split_str_push(iter: impl Iterator<Item = char>) -> String {
    iter.enumerate().fold(String::new(), |mut s, (p, c)| {
        if p % 5 == 0 && p > 0 {
            s.push(' ');
        }
        s.push(c);
        s
    })
}
