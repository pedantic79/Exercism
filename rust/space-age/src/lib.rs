use std::ops::Div;
use std::ops::Mul;

#[derive(Debug)]
pub struct Duration {
    seconds: f64,
}

impl From<u64> for Duration {
    fn from(s: u64) -> Self {
        Duration { seconds: s as f64 }
    }
}

impl From<f64> for Duration {
    fn from(f: f64) -> Self {
        Duration { seconds: f }
    }
}

impl<'a> Div<Duration> for &'a Duration {
    type Output = f64;

    fn div(self, rhs: Duration) -> f64 {
        self.seconds / rhs.seconds
    }
}

#[derive(Debug)]
pub struct OrbitalFactor {
    factor: f64,
}

impl From<f64> for OrbitalFactor {
    fn from(f: f64) -> Self {
        OrbitalFactor { factor: f }
    }
}

impl Mul<Duration> for OrbitalFactor {
    type Output = Duration;

    fn mul(self, rhs: Duration) -> Duration {
        Duration::from(self.factor * rhs.seconds)
    }
}

pub trait Planet {
    fn orbital_factor() -> OrbitalFactor;
    fn orbital_duration() -> Duration {
        Self::orbital_factor() * Earth::orbital_duration()
    }

    fn years_during(d: &Duration) -> f64 {
        d / Self::orbital_duration()
    }
}

pub struct Mercury;
pub struct Venus;
pub struct Earth;
pub struct Mars;
pub struct Jupiter;
pub struct Saturn;
pub struct Uranus;
pub struct Neptune;

impl Planet for Mercury {
    fn orbital_factor() -> OrbitalFactor {
        OrbitalFactor::from(0.240_846_7)
    }
}

impl Planet for Venus {
    fn orbital_factor() -> OrbitalFactor {
        OrbitalFactor::from(0.615_197_26)
    }
}

impl Planet for Earth {
    fn orbital_duration() -> Duration {
        Duration::from(31_557_600)
    }

    fn orbital_factor() -> OrbitalFactor {
        OrbitalFactor::from(1.0)
    }
}

impl Planet for Mars {
    fn orbital_factor() -> OrbitalFactor {
        OrbitalFactor::from(1.880_815_8)
    }
}

impl Planet for Jupiter {
    fn orbital_factor() -> OrbitalFactor {
        OrbitalFactor::from(11.862_615)
    }
}

impl Planet for Saturn {
    fn orbital_factor() -> OrbitalFactor {
        OrbitalFactor::from(29.447_498)
    }
}

impl Planet for Uranus {
    fn orbital_factor() -> OrbitalFactor {
        OrbitalFactor::from(84.016_846)
    }
}

impl Planet for Neptune {
    fn orbital_factor() -> OrbitalFactor {
        OrbitalFactor::from(164.791_32)
    }
}
