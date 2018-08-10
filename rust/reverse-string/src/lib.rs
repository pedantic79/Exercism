// Not sure if I'm "allowed" to import random crates.
// What I learned that unicode-segmentation is the way to go,
// But honestly, it's stupid for a "modern" language to have no built-in
// or std-library way to iterate over the graphemes. (i.e. golang runes)

extern crate unicode_segmentation;

use unicode_segmentation::UnicodeSegmentation;

pub fn reverse(input: &str) -> String {
    UnicodeSegmentation::graphemes(input, true).rev().collect()
}
