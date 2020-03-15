#include "hexadecimal.h"
#include <cctype>
#include <exception>
#include <numeric>
#include <string>

using namespace std;

int to_digit(char c) {
    auto uc = static_cast<unsigned char>(c);
    if (isdigit(uc)) {
        return c - '0';
    } else if (isxdigit(uc)) {
        return tolower(uc) - 'a' + 10;
    } else {
        throw invalid_argument("invalid char");
    }
}

int hexadecimal::convert(string_view num_str) {
    try {
        return accumulate(
            next(num_str.begin()), num_str.end(), to_digit(num_str.front()),
            [](int total, const auto &c) { return total * 16 + to_digit(c); });
    } catch (const invalid_argument &e) {
        return 0;
    }
}
