use std::ops::Div;
use std::ops::Mul;

#[derive(Debug)]
pub struct Duration {
    seconds: f64,
}

impl From<u64> for Duration {
    fn from(seconds: u64) -> Self {
        Duration {
            seconds: seconds as f64,
        }
    }
}

impl From<f64> for Duration {
    fn from(seconds: f64) -> Self {
        Duration { seconds }
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

#[macro_export]
macro_rules! planet {
    ($name:tt, $factor:literal) => {
        pub struct $name;

        impl Planet for $name {
            fn orbital_factor() -> OrbitalFactor {
                OrbitalFactor::from($factor)
            }
        }
    };

    ($name:tt, $factor:literal, $duration:literal) => {
        pub struct $name;

        impl Planet for $name {
            fn orbital_factor() -> OrbitalFactor {
                OrbitalFactor::from($factor)
            }
            fn orbital_duration() -> Duration {
                Duration::from($duration)
            }
        }
    };
}

planet!(Mercury, 0.240_846_7);
planet!(Venus, 0.615_197_26);
planet!(Earth, 1.0, 31_557_600);
planet!(Mars, 1.880_815_8);
planet!(Jupiter, 11.862_615);
planet!(Saturn, 29.447_498);
planet!(Uranus, 84.016_846);
planet!(Neptune, 164.791_320);
