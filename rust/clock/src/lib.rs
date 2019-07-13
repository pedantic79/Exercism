use std::fmt;

#[derive(Debug, Eq, PartialEq)]
pub struct Clock {
    minutes: i32,
}

const MINUTES_PER_DAY: i32 = 60 * 24;

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let hrs = self.minutes / 60;
        let mnts = self.minutes % 60;
        write!(f, "{:02}:{:02}", hrs, mnts)
    }
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let minutes = Self::adjust(hours * 60 + minutes);
        Clock { minutes }
    }

    pub fn add_minutes(self, minutes: i32) -> Self {
        Clock::new(0, self.minutes + minutes)
    }

    fn adjust(minutes: i32) -> i32 {
        if minutes < 0 {
            // Determine how many times we need to add 1440 to make `minutes` positive
            let magnitute = minutes.abs() / MINUTES_PER_DAY + 1;
            minutes + MINUTES_PER_DAY * magnitute
        } else {
            minutes % MINUTES_PER_DAY
        }
    }
}
