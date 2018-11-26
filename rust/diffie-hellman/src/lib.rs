extern crate rand;

use rand::{thread_rng, Rng};

pub fn private_key(p: u64) -> u64 {
    thread_rng().gen_range(2, p)
}

pub fn public_key(p: u64, g: u64, a: u64) -> u64 {
    modular_exponentiation(g, a, p)
}

pub fn secret(p: u64, b_pub: u64, a: u64) -> u64 {
    modular_exponentiation(b_pub, a, p)
}

fn modular_exponentiation(base: u64, exponent: u64, modulus: u64) -> u64 {
    match modulus {
        1 => 0,
        _ => {
            let mut result = 1;
            let mut b = base % modulus;
            let mut e = exponent;

            while e > 0 {
                if e % 2 == 1 {
                    result = (result * b) % modulus;
                }
                e >>= 1;
                b = (b * b) % modulus;
            }
            result
        }
    }
}
