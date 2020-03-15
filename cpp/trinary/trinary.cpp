#include "trinary.h"
#include <cctype>
#include <exception>
#include <numeric>
#include <string>

using namespace std;

int to_digit(char c) {
    auto uc = static_cast<unsigned char>(c);
    if (isdigit(uc) && uc < '3') {
        return c - '0';
    } else {
        throw invalid_argument("invalid char");
    }
}

int trinary::to_decimal(string_view num_str) {
    try {
        return accumulate(
            num_str.begin(), num_str.end(), 0,
            [](int total, const auto &c) { return total * 3 + to_digit(c); });
    } catch (const invalid_argument &e) {
        return 0;
    }
}
