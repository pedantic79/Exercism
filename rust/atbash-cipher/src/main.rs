use atbash_cipher::*;
use easybench::bench;

fn main() {
    for len in &[12, 48, 252, 1024] {
        let input = String::from_utf8(vec![b'X'; *len as usize]).unwrap();

        println!(
            "flat_map {:4}: {}",
            len,
            bench(|| split_flat_map(input.chars()))
        );
        println!(
            "from_fn {:5}: {}",
            len,
            bench(|| split_from_fn(input.chars()))
        );
    }
}
