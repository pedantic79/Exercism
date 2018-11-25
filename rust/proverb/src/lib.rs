pub fn build_proverb(list: &[&str]) -> String {
    if list.is_empty() {
        String::new()
    } else {
        list.windows(2)
            .map(proverb)
            .chain(std::iter::once(format!(
                "And all for the want of a {}.",
                list[0]
            ))).collect::<Vec<_>>()
            .join("\n")
    }
}

fn proverb(a: &[&str]) -> String {
    debug_assert_eq!(a.len(), 2);

    format!("For want of a {} the {} was lost.", a[0], a[1])
}
