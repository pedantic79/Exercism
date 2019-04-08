use std::collections::BTreeMap;

pub fn transform(h: &BTreeMap<i32, Vec<char>>) -> BTreeMap<char, i32> {
    let mut result = BTreeMap::new();

    for (count, letters) in h {
        for letter in letters {
            if let Some(l) = letter.to_lowercase().next() {
                *result.entry(l).or_insert(0) += count;
            }
        }
    }
    result
}
