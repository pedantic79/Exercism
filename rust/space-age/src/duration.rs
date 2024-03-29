use std::ops::Div;

#[derive(Debug)]
pub struct Duration {
    seconds: f64,
}

impl From<u64> for Duration {
    fn from(seconds: u64) -> Self {
        Self::from(seconds as f64)
    }
}

impl From<f64> for Duration {
    fn from(seconds: f64) -> Self {
        Duration { seconds }
    }
}

impl Div<Duration> for &Duration {
    type Output = f64;

    fn div(self, rhs: Duration) -> Self::Output {
        self.seconds / rhs.seconds
    }
}
