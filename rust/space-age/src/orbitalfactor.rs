use super::duration::Duration;
use std::ops::Mul;

#[derive(Debug)]
pub struct OrbitalFactor {
    factor: f64,
}

impl From<f64> for OrbitalFactor {
    fn from(f: f64) -> Self {
        OrbitalFactor { factor: f }
    }
}

impl Mul<f64> for OrbitalFactor {
    type Output = Duration;

    fn mul(self, rhs: f64) -> Duration {
        Duration::from(self.factor * rhs)
    }
}
