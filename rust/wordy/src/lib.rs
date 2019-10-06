enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

struct Tokens<'a>(std::iter::Peekable<std::str::SplitWhitespace<'a>>);

impl<'a> Tokens<'a> {
    pub fn new(input: &'a str) -> Self {
        let p = input.split_whitespace().peekable();
        Self(p)
    }

    fn check(&mut self, word: &str) -> Option<bool> {
        self.0.next().map(|w| w == word)
    }

    fn parse_number(&mut self) -> Option<i32> {
        self.0.next()?.trim_end_matches('?').parse().ok()
    }

    fn parse_ordinal(&mut self) -> Option<u32> {
        parse_ordinal(self.0.next()?)
    }

    fn empty(&mut self) -> bool {
        self.0.peek().is_none()
    }

    fn parse_operation(&mut self) -> Option<Operation> {
        let word = self.0.next()?;

        match word {
            "plus" => Some(Operation::Add),
            "minus" => Some(Operation::Sub),
            "multiplied" => Some(Operation::Mul),
            "divided" => Some(Operation::Div),
            "raised" => Some(Operation::Pow),
            _ => None,
        }
    }
}

pub fn answer(command: &str) -> Option<i32> {
    let mut tokens = Tokens::new(command);
    let mut answer: i32;

    tokens.check("What")?;
    tokens.check("is")?;

    answer = tokens.parse_number()?;
    while !tokens.empty() {
        answer = match tokens.parse_operation()? {
            Operation::Add => answer + tokens.parse_number()?,
            Operation::Sub => answer - tokens.parse_number()?,
            Operation::Mul => {
                tokens.check("by")?;
                answer * tokens.parse_number()?
            }
            Operation::Div => {
                tokens.check("by")?;
                answer / tokens.parse_number()?
            }
            Operation::Pow => {
                tokens.check("to")?;
                tokens.check("the")?;
                let exp = tokens.parse_ordinal()?;
                tokens.check("power?");
                answer.pow(exp)
            }
        };
    }

    Some(answer)
}

fn parse_ordinal(s: &str) -> Option<u32> {
    if s.len() < 3 {
        None
    } else {
        let (left, right) = s.split_at(s.len() - 2);

        let num = left.parse().ok()?;
        let tens = (num % 100) / 10;

        match (num % 10, right) {
            (0, "th") | (1, "st") | (2, "nd") | (3, "rd") if tens != 1 => Some(num),
            (n, "th") if n > 3 && tens != 1 => Some(num),
            (_, "th") if tens == 1 => Some(num),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::parse_ordinal;
    #[test]
    fn test_valid_ordinals() {
        let ordinals = [
            "1st", "2nd", "3rd", "4th", "9th", "10th", "11th", "12th", "13th", "20th", "21st",
            "22nd", "23rd", "24th", "30th", "31st", "101st", "111th", "121st",
        ];

        for o in ordinals.iter() {
            assert!(parse_ordinal(o).is_some())
        }
    }

    #[test]
    fn test_invalid_ordinals() {
        let ordinals = [
            "1th", "2th", "3th", "4st", "9nd", "10rd", "11st", "12nd", "13rd", "20st", "21th",
            "22th", "23th", "24rd", "30nd", "31th", "101th", "111st", "121th", "20fj", "1jkl",
        ];

        for o in ordinals.iter() {
            assert!(parse_ordinal(o).is_none())
        }
    }
}
