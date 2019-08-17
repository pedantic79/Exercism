use atbash_cipher::*;
use easybench::bench;

macro_rules! benchmark {
    ($name:literal, $ben:ident, $input:ident) => {
        println!(
            "{:>9}/{:<4}{}",
            $name,
            $input.len(),
            bench(|| $ben($input.chars()))
        );
    };
}

fn main() {
    for len in &[12, 48, 252, 1024] {
        let input = String::from_utf8(vec![b'X'; *len as usize]).unwrap();

        benchmark!("vec", split_vec, input);
        benchmark!("iterator", split_flat_map, input);
        benchmark!("fold_str", split_str_push, input);
        benchmark!("from_fn", split_from_fn, input);
    }
}

#[allow(dead_code)]
fn split_vec(iter: impl Iterator<Item = char>) -> String {
    let v = iter.collect::<Vec<char>>();
    v.chunks(5)
        .map(|s| s.iter().collect())
        .collect::<Vec<String>>()
        .join(" ")
}

#[allow(dead_code)]
fn split_str_push(iter: impl Iterator<Item = char>) -> String {
    iter.enumerate().fold(String::new(), |mut s, (p, c)| {
        if p % 5 == 0 && p > 0 {
            s.push(' ');
        }
        s.push(c);
        s
    })
}
