#include "luhn.h"
#include <cctype>
#include <numeric>

using namespace std;

bool is_digit(char c) {
    auto uc = static_cast<unsigned char>(c);
    return isdigit(uc);
}

int double_int(int num) {
    int doubled = num * 2;
    return (doubled > 9) ? doubled - 9 : doubled;
}

bool luhn::valid(string_view input) {
    int position = 0;
    int luhn_sum = 0;

    for (auto c_itr = input.crbegin(); c_itr != input.crend(); c_itr++) {
        auto c = *c_itr;
        if (is_digit(c)) {
            ++position;
            auto value = c - '0';
            luhn_sum += (position % 2 == 0) ? double_int(value) : value;
        } else if (c != ' ') {
            return false;
        }
    }

    return luhn_sum % 10 == 0 && position > 1;
}
