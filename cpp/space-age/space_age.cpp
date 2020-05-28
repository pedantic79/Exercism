#include "space_age.h"

namespace space_age {

namespace {
constexpr double earthYearsPerSecond = 1.0 / 31'557'600.0;
constexpr double mercuryFactor = 0.240'846'7;
constexpr double venusFactor = 0.615'197'26;
constexpr double marsFactor = 1.880'815'8;
constexpr double jupiterFactor = 11.862'615;
constexpr double saturnFactor = 29.447'498;
constexpr double uranusFactor = 84.016'846;
constexpr double neptuneFactor = 164.791'32;
} // namespace

space_age::space_age(unsigned long long secs) : mSeconds{secs} {}

unsigned long long space_age::seconds() const { return mSeconds; }

double space_age::on_earth() const {
    return static_cast<double>(mSeconds) * earthYearsPerSecond;
}

double space_age::on_mercury() const { return on_earth() / mercuryFactor; }

double space_age::on_venus() const { return on_earth() / venusFactor; }

double space_age::on_mars() const { return on_earth() / marsFactor; }

double space_age::on_jupiter() const { return on_earth() / jupiterFactor; }

double space_age::on_saturn() const { return on_earth() / saturnFactor; }

double space_age::on_uranus() const { return on_earth() / uranusFactor; }

double space_age::on_neptune() const { return on_earth() / neptuneFactor; }

} // namespace space_age
