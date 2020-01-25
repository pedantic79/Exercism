mod duration;
mod orbitalfactor;

pub use duration::Duration;
use orbitalfactor::OrbitalFactor;

const EARTH_ORBITAL_DURATION: u64 = 31_557_600;

pub trait Planet {
    const ORBITAL_FACTOR: f64;

    fn orbital_duration() -> Duration {
        OrbitalFactor::from(Self::ORBITAL_FACTOR) * Duration::from(EARTH_ORBITAL_DURATION)
    }

    fn years_during(d: &Duration) -> f64 {
        d / Self::orbital_duration()
    }
}

#[macro_export]
macro_rules! planet {
    ($name:tt, $factor:expr) => {
        pub struct $name;

        impl Planet for $name {
            const ORBITAL_FACTOR: f64 = $factor;
        }
    };
}

planet!(Mercury, 0.240_846_7);
planet!(Venus, 0.615_197_26);
planet!(Earth, 1.0);
planet!(Mars, 1.880_815_8);
planet!(Jupiter, 11.862_615);
planet!(Saturn, 29.447_498);
planet!(Uranus, 84.016_846);
planet!(Neptune, 164.791_320);
