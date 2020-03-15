#include "armstrong_numbers.h"
#include <cmath>
#include <numeric>
#include <string>

bool armstrong_numbers::is_armstrong_number(int num) {
    std::string s = std::to_string(num);
    int exp = s.length();

    return std::accumulate(s.begin(), s.end(), 0,
                           [exp](auto total, const auto &c) {
                               return total + std::pow(c - '0', exp);
                           }) == num;
}
