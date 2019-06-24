#include "raindrops.h"
#include <sstream>

using namespace std;

bool divisible_by(unsigned int a, unsigned int b) { return a % b == 0; }

string raindrops::convert(unsigned int number) {
    ostringstream oss;

    if (divisible_by(number, 3)) {
        oss << "Pling";
    }

    if (divisible_by(number, 5)) {
        oss << "Plang";
    }

    if (divisible_by(number, 7)) {
        oss << "Plong";
    }

    // Check if our stringstream is empty
    if (oss.tellp() == 0) {
        oss << number;
    }

    return oss.str();
}
