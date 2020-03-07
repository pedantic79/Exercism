use std::convert::TryInto;
use std::fmt;

#[derive(Debug, Eq, PartialEq)]
pub struct Clock(u16);

const MINUTES_PER_DAY: i32 = 60 * 24;

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (hrs, mnts) = Self::m_to_hm(self.0);
        write!(f, "{:02}:{:02}", hrs, mnts)
    }
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        Clock(Self::hm_to_m(hours, minutes))
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Self::new(0, i32::from(self.0) + minutes)
    }

    // As of Rust 1.38, there is now a eucliedan division and remainder
    fn hm_to_m(hours: i32, minutes: i32) -> u16 {
        let minutes = hours * 60 + minutes;
        minutes
            .rem_euclid(MINUTES_PER_DAY)
            .try_into()
            .expect("expected range is [0 to MINUTES_PER_DAY)")
    }

    fn m_to_hm(minutes: u16) -> (u16, u16) {
        (minutes / 60, minutes % 60)
    }
}
