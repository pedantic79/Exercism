use std::fmt;

#[derive(Debug, Eq, PartialEq)]
pub struct Clock {
    minutes: i32,
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let hrs = self.minutes / 60;
        let mnts = self.minutes % 60;
        write!(f, "{:02}:{:02}", hrs, mnts)
    }
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let mnts = hours * 60 + minutes;
        Clock {
            minutes: if mnts < 0 {
                let magnitute = mnts.abs() / 1440 + 1;
                mnts + 1440 * magnitute
            } else {
                mnts
            } % 1440,
        }
    }

    pub fn add_minutes(self, minutes: i32) -> Self {
        Clock::new(0, self.minutes + minutes)
    }
}
