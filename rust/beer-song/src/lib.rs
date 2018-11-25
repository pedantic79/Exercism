pub fn verse(n: i32) -> String {
    format!(
        r#"{} of beer on the wall, {} of beer.
{}, {} of beer on the wall.
"#,
        to_capitialized(&bottles(n)),
        bottles(n),
        action(n),
        left(n)
    )
}

fn bottles(n: i32) -> String {
    match n {
        0 => "no more bottles".to_string(),
        1 => "1 bottle".to_string(),
        _ => format!("{} bottles", n),
    }
}

fn action(n: i32) -> &'static str {
    match n {
        0 => "Go to the store and buy some more",
        1 => "Take it down and pass it around",
        _ => "Take one down and pass it around",
    }
}

fn left(n: i32) -> String {
    let remainder = n - 1;
    match n {
        0 => "99 bottles".to_string(),
        1 => "no more bottles".to_string(),
        _ if remainder == 1 => "1 bottle".to_string(),
        _ => format!("{} bottles", remainder),
    }
}

fn to_capitialized(s: &str) -> String {
    let mut c = s.chars();

    match c.next() {
        Some(f) => f.to_uppercase().chain(c).collect(),
        None => String::new(),
    }
}

pub fn sing(start: i32, end: i32) -> String {
    assert!(start > end);

    (end..=start)
        .rev()
        .map(verse)
        .collect::<Vec<_>>()
        .join("\n")
}
