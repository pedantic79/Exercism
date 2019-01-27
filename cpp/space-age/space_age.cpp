#include "space_age.h"

namespace space_age {

namespace {
constexpr double earthYearsPerSecond = 1.0 / 31557600.0;
const double mercuryFactor = 0.2408467;
const double venusFactor = 0.61519726;
const double marsFactor = 1.8808158;
const double jupiterFactor = 11.862615;
const double saturnFactor = 29.447498;
const double uranusFactor = 84.016846;
const double neptuneFactor = 164.79132;
} // namespace

space_age::space_age(unsigned long long secs) : mSeconds(secs) {}

unsigned long long space_age::seconds() const { return mSeconds; }

double space_age::on_earth() const { return mSeconds * earthYearsPerSecond; }

double space_age::on_mercury() const { return on_earth() / mercuryFactor; }

double space_age::on_venus() const { return on_earth() / venusFactor; }

double space_age::on_mars() const { return on_earth() / marsFactor; }

double space_age::on_jupiter() const { return on_earth() / jupiterFactor; }

double space_age::on_saturn() const { return on_earth() / saturnFactor; }

double space_age::on_uranus() const { return on_earth() / uranusFactor; }

double space_age::on_neptune() const { return on_earth() / neptuneFactor; }

} // namespace space_age
