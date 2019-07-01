use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::one_of,
    combinator::{all_consuming, map, opt, verify},
    error::ErrorKind,
    multi::{count, many0},
    sequence::{pair, preceded, terminated},
    Err::Error,
    IResult,
};

pub fn number(user_number: &str) -> Option<String> {
    nanp_number(user_number).ok().map(|(_, x)| x)
}

// Matches +1 or 1
fn prefix(s: &str) -> IResult<&str, &str> {
    alt((tag("+1"), tag("1")))(s)
}

// Matches zero or more valid seperators [space, ., -]
fn seperator(s: &str) -> IResult<&str, ()> {
    map(many0(one_of(" .-")), |_| ())(s)
}

// Matches a seperator preceded and succeeded digit
fn digit(s: &str) -> IResult<&str, char> {
    terminated(preceded(seperator, one_of("0123456789")), seperator)(s)
}

// Matches area code with optional prefix and matching parens
fn area_code(s: &str) -> IResult<&str, String> {
    let (s, _) = preceded(opt(prefix), seperator)(s)?;

    let (s, t) = opt(tag("("))(s)?;
    let lparens = t.is_some();

    let (pre, numbers) = num_sequence(3, s)?;

    let (s, t) = opt(tag(")"))(pre)?;
    let rparens = t.is_some();

    if lparens == rparens {
        Ok((s, numbers))
    } else {
        Err(Error((pre, ErrorKind::Tag)))
    }
}

// Matches 7 digit sequence
fn main_number(s: &str) -> IResult<&str, String> {
    num_sequence(7, s)
}

// Matches n digit sequence and verifies that it does not start with a 0 or 1
fn num_sequence(n: usize, s: &str) -> IResult<&str, String> {
    verify(
        map(count(digit, n), |v| v.into_iter().collect()),
        |n: &String| !(n.starts_with('0') || n.starts_with('1')),
    )(s)
}

// Matches area_code then main_number, and concats them together
fn nanp_number(s: &str) -> IResult<&str, String> {
    all_consuming(map(pair(area_code, main_number), |(code, num)| {
        format!("{}{}", code, num)
    }))(s)
}

#[cfg(test)]
mod tests {
    use nom::{error::ErrorKind, Err::Error};

    #[test]
    fn area_code_test() {
        use super::area_code;
        assert_eq!(area_code("223"), Ok(("", String::from("223"))));
        assert_eq!(area_code("(223"), Err(Error(("", ErrorKind::Tag))));
        assert_eq!(area_code("(223)"), Ok(("", String::from("223"))));
        assert_eq!(area_code("(2.2.3)"), Ok(("", String::from("223"))));
        assert_eq!(area_code("223)"), Err(Error((")", ErrorKind::Tag))));
        assert_eq!(
            area_code("(1234)"),
            Err(Error(("1234)", ErrorKind::Verify)))
        );
    }

    #[test]
    fn area_code_prefix_test() {
        use super::area_code;
        assert_eq!(area_code("+1 223"), Ok(("", String::from("223"))));
        assert_eq!(area_code("1 (223)"), Ok(("", String::from("223"))));
        assert_eq!(area_code("+1 (223)"), Ok(("", String::from("223"))));
        assert_eq!(area_code("1223"), Ok(("", String::from("223"))));
        assert_eq!(area_code("+1223"), Ok(("", String::from("223"))));
        assert_eq!(
            area_code("(1234)"),
            Err(Error(("1234)", ErrorKind::Verify)))
        );
        assert_eq!(area_code("1234)"), Err(Error((")", ErrorKind::Tag))));
        assert_eq!(
            area_code("+2 234"),
            Err(Error(("+2 234", ErrorKind::OneOf)))
        );
        assert_eq!(area_code("+1 034"), Err(Error(("034", ErrorKind::Verify))));
        assert_eq!(area_code("2034"), Ok(("4", String::from("203"))));
    }

    #[test]
    fn number_test() {
        use super::main_number;
        assert_eq!(main_number(" 456-7890"), Ok(("", String::from("4567890"))));
        assert_eq!(
            main_number(" 456    7890  "),
            Ok(("", String::from("4567890")))
        );
        assert_eq!(
            main_number(" 45.6-7...89..0..."),
            Ok(("", String::from("4567890")))
        );
        assert_eq!(
            main_number("0567890"),
            Err(Error(("0567890", ErrorKind::Verify)))
        );
    }
}
