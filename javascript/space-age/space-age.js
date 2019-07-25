const EARTH_YEAR = 31557600;

const ORBITAL_PERIODS = {
  "earth": 1.00000000,
  "mercury": 0.24084670,
  "venus": 0.61519726,
  "mars": 1.88081580,
  "jupiter": 11.86261500,
  "saturn": 29.44749800,
  "uranus": 84.01684600,
  "neptune": 164.79132000,
};

export const age = (planet, seconds) => {
  return nearestHundredths(seconds / EARTH_YEAR / ORBITAL_PERIODS[planet]);
};

const nearestHundredths = (num) => Math.round(100 * num) / 100;
