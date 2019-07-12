pub struct Brackets<'a> {
    input_str: &'a str,
}

impl<'a> From<&'a str> for Brackets<'a> {
    fn from(input: &'a str) -> Self {
        Brackets { input_str: input }
    }
}

impl<'a> Brackets<'a> {
    pub fn are_balanced(&self) -> bool {
        const VALID: [char; 6] = ['(', ')', '[', ']', '{', '}'];
        let mut stack: Vec<char> = Vec::new();

        for bracket in self.input_str.chars().filter(|x| VALID.contains(x)) {
            if let Some(right) = get_match(bracket) {
                stack.push(right);
            } else if stack.pop() != Some(bracket) {
                return false;
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
