pub fn build_proverb(list: &[&str]) -> String {
    let l = list.len();

    if l == 0 {
        String::new()
    } else {
        let mut proverbs = Vec::with_capacity(l);

        for w in list.windows(2) {
            proverbs.push(proverb(w));
        }
        proverbs.push(format!("And all for the want of a {}.", list[0]));

        debug_assert_eq!(proverbs.len(), l);
        proverbs.join("\n")
    }
}

fn proverb(a: &[&str]) -> String {
    debug_assert_eq!(a.len(), 2);

    format!("For want of a {} the {} was lost.", a[0], a[1])
}
