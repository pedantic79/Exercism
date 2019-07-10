pub fn reply(message: &str) -> &str {
    let message = message.trim();

    if message == "" {
        "Fine. Be that way!"
    } else {
        let yell = is_yell(message);
        let question = message.ends_with('?');

        match (yell, question) {
            (true, true) => "Calm down, I know what I'm doing!",
            (true, false) => "Whoa, chill out!",
            (false, true) => "Sure.",
            (false, false) => "Whatever.",
        }
    }
}

/// is_yell checks to see if the message is all uppercase. If it happens to also
/// be the same as it is all lowercase, then it returns false
fn is_yell(message: &str) -> bool {
    message == message.to_uppercase() && message != message.to_lowercase()
}
