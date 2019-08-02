use std::cmp::Ordering;

pub fn find<C, T>(container: C, key: T) -> Option<usize>
where
    C: AsRef<[T]>,
    T: Ord,
{
    let mut container = container.as_ref();
    let mut offset = 0;

    loop {
        let midpoint = container.len() / 2;
        let (left, right) = container.split_at(midpoint);

        if let Some(value) = right.first() {
            match value.cmp(&key) {
                Ordering::Less => {
                    container = &right[1..];
                    offset += left.len() + 1;
                }
                Ordering::Equal => return Some(midpoint + offset),
                Ordering::Greater => container = left,
            }
        } else {
            return None;
        }
    }
}
