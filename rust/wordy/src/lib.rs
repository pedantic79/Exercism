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
        let s = self.0.next()?;
        let (left, right) = s.split_at(s.len() - 2);

        let num = left.parse().ok()?;
        match (num % 10, right) {
            (1, "st") | (2, "nd") | (3, "rd") => Some(num),
            (_, "th") => Some(num),
            _ => None,
        }
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
