#![allow(dead_code)]

use atbash_cipher::*;
use easybench::bench;

macro_rules! benchmark {
    ($name:literal, $ben:ident, $input:ident) => {
        println!(
            "{:>9}/{:<4}{}",
            $name,
            $input.len(),
            bench(|| $ben($input.chars(), $input.len()))
        );
    };
}

fn main() {
    for len in &[12, 48, 252, 1024] {
        let input = String::from_utf8(vec![b'X'; *len as usize]).unwrap();

        benchmark!("vec", vec, input);
        benchmark!("iterator", iterator, input);
        benchmark!("push", push, input);
        benchmark!("from_fn", from_fn, input);
        benchmark!("push_wcap", push_wcap, input);
        benchmark!("fmfn_wcap", from_fn_wcap, input);
        benchmark!("foo", iter_wcap, input);
    }
}

fn vec(iter: impl Iterator<Item = char>, _size: usize) -> String {
    let v = iter.collect::<Vec<char>>();
    v.chunks(5)
        .map(|s| s.iter().collect())
        .collect::<Vec<String>>()
        .join(" ")
}

#[inline(always)]
fn iterator(iter: impl Iterator<Item = char>, _size: usize) -> String {
    split_flat_map(iter)
}

fn push(iter: impl Iterator<Item = char>, _size: usize) -> String {
    iter.enumerate().fold(String::new(), |mut s, (p, c)| {
        if p % 5 == 0 && p > 0 {
            s.push(' ');
        }
        s.push(c);
        s
    })
}

#[inline(always)]
fn from_fn(iter: impl Iterator<Item = char>, _size: usize) -> String {
    split_from_fn(iter)
}

// The size here is correct for this test, but when dealing with atbash
// we won't know the length of the iterator. We could call String::shrink_to_fit()
fn push_wcap(iter: impl Iterator<Item = char>, size: usize) -> String {
    iter.enumerate()
        .fold(String::with_capacity(size * 6 / 5), |mut s, (p, c)| {
            if p % 5 == 0 && p > 0 {
                s.push(' ');
            }
            s.push(c);
            s
        })
}

fn from_fn_wcap(iter: impl Iterator<Item = char>, size: usize) -> String {
    let mut count = 0;
    let mut iter = iter.peekable();

    let mut s = String::with_capacity(size * 6 / 5);
    s.extend(std::iter::from_fn(move || {
        if count == 5 && iter.peek().is_some() {
            count = 0;
            Some(' ')
        } else {
            count += 1;
            iter.next()
        }
    }));
    s
}

fn iter_wcap(iter: impl Iterator<Item = char>, size: usize) -> String {
    let mut count = 0;
    let mut s = String::with_capacity(size * 6 / 5);
    let mut iter = iter.peekable();

    while let Some(c) = iter.next() {
        if count == 5 && iter.peek().is_some() {
            s.push(' ');
            s.push(c);
            count = 1;
        } else {
            count += 1;
            s.push(c);
        }
    }

    s
}
