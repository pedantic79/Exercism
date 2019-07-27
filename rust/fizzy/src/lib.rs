use std::ops::Rem;

type Pred<T> = Box<Fn(T) -> bool>;

pub struct Matcher<T> {
    predicate: Pred<T>,
    word: String,
}

impl<T> Matcher<T> {
    pub fn new<F, S>(matcher: F, subs: S) -> Matcher<T>
    where
        F: Fn(T) -> bool + 'static,
        S: AsRef<str>,
    {
        Self {
            predicate: Box::new(matcher),
            word: subs.as_ref().to_string(),
        }
    }
}

#[derive(Default)]
pub struct Fizzy<T> {
    matchers: Vec<Matcher<T>>,
}

impl<T> Fizzy<T>
where
    T: Copy + Default + ToString,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_matcher(self, matcher: Matcher<T>) -> Self {
        let mut v = self.matchers;
        v.push(matcher);
        Self { matchers: v }
    }

    /// map this fizzy onto every element of an interator, returning a new iterator
    pub fn apply<I>(self, iter: I) -> impl Iterator<Item = String>
    where
        I: Iterator<Item = T>,
    {
        iter.map(move |item| self.fizzbuzz(item))
    }

    fn fizzbuzz(&self, item: T) -> String {
        let s = self
            .matchers
            .iter()
            .filter(|matcher| (matcher.predicate)(item))
            .map(|matcher| matcher.word.clone())
            .collect::<String>();

        if s.is_empty() {
            item.to_string()
        } else {
            s
        }
    }
}

/// convenience function: return a Fizzy which applies the standard fizz-buzz rules
pub fn fizz_buzz<T>() -> Fizzy<T>
where
    T: Copy + Default + From<u8> + PartialEq + Rem<Output = T> + ToString,
{
    Fizzy::new()
        .add_matcher(Matcher::new(|n: T| n % 3.into() == T::default(), "fizz"))
        .add_matcher(Matcher::new(|n: T| n % 5.into() == T::default(), "buzz"))
}
