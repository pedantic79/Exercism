/// Checks if brackets are balanced. The previous version of this problem was
/// known as bracket_push and involved learning about structs and lifetimes.
/// This new version is greatly simplified.
///
/// Rather than rewriting everything, I am just calling the previous code.
///
pub fn brackets_are_balanced(s: &str) -> bool {
    Brackets::from(s).are_balanced()
}

pub struct Brackets<'a> {
    data: &'a str,
}

impl<'a> From<&'a str> for Brackets<'a> {
    fn from(data: &'a str) -> Self {
        Brackets { data }
    }
}

impl Brackets<'_> {
    pub fn are_balanced(&self) -> bool {
        const VALID: [char; 6] = ['(', ')', '[', ']', '{', '}'];
        let mut stack: Vec<char> = Vec::new();

        for bracket in self.data.chars().filter(|x| VALID.contains(x)) {
            if let Some(right) = get_match(bracket) {
                stack.push(right);
            } else if Some(bracket) != stack.pop() {
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
