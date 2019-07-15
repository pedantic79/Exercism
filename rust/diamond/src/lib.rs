pub fn get_diamond(c: char) -> Vec<String> {
    let count = (c as u8) - b'A';

    let top = (0..=count).map(|n| {
        let middle = 2 * i32::from(n) - 1;
        let letter = (b'A' + n) as char;

        gen(middle, count - n, letter)
    });
    let bottom = top.clone().rev().skip(1);

    top.chain(bottom).collect()
}

fn gen(middle: i32, outside: u8, c: char) -> String {
    let outside = outside as usize;

    if middle > 0 {
        format!(
            "{0}{1}{2}{1}{0}",
            " ".repeat(outside),
            c,
            " ".repeat(middle as usize)
        )
    } else {
        format!("{0}{1}{0}", " ".repeat(outside), c)
    }
}
