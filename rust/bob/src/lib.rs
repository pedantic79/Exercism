pub fn reply(message: &str) -> &str {
    match message.trim() {
        "" => "Fine. Be that way!",
        m if is_yell(m) && m.ends_with('?') => "Calm down, I know what I'm doing!",
        m if is_yell(m) => "Whoa, chill out!",
        m if m.ends_with('?') => "Sure.",
        _ => "Whatever.",
    }
}

fn is_yell(message: &str) -> bool {
    let uc = message.to_uppercase();
    let lc = message.to_lowercase();

    uc == message && uc != lc
}
