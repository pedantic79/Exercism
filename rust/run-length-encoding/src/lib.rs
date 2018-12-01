extern crate itertools;

use itertools::Itertools;
use std::cmp;

pub fn encode(source: &str) -> String {
    source
        .chars()
        .group_by(|&x| x)
        .into_iter()
        .map(|(key, group)| {
            let l = group.count();
            if l > 1 {
                format!("{}{}", l, key)
            } else {
                format!("{}", key)
            }
        }).collect()
}

pub fn decode(source: &str) -> String {
    source
        .chars()
        .fold((String::new(), 0), |(mut output, count), x| {
            if let Some(d) = x.to_digit(10) {
                (output, count * 10 + d as usize)
            } else {
                output.push_str(&x.to_string().repeat(cmp::max(count, 1)));
                (output, 0)
            }
        }).0
}
