pub struct Brackets {
    brackets: Vec<char>,
}

impl<'a> From<&'a str> for Brackets {
    fn from(input: &str) -> Self {
        let valid = vec!['(', ')', '[', ']', '{', '}'];
        Brackets::new(input.chars().filter(|x| valid.contains(x)).collect())
    }
}

impl Brackets {
    pub fn new(bracket_chars: Vec<char>) -> Self {
        Brackets { brackets: bracket_chars }
    }

    pub fn are_balanced(&self) -> bool {
        let mut stack: Vec<char> = Vec::new();

        for bracket in &self.brackets {
            if let Some(last) = stack.pop() {
                if get_match(last) != Some(*bracket) {
                    stack.push(last);
                    stack.push(*bracket);
                }
            } else {
                stack.push(*bracket);
            }
        }

        stack.is_empty()
    }
}

fn get_match(bracket: char) -> Option<char> {
    match bracket {
        '(' => Some(')'),
        '{' => Some('}'),
        '[' => Some(']'),
        _ => None,
    }
}
