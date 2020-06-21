#include "series.h"
#include <algorithm>
#include <stdexcept>

// This requires C++17
std::vector<int> series::digits(std::string_view s) {
    std::vector<int> v;

    std::transform(s.begin(), s.end(), std::back_inserter(v),
                   [](auto c) { return c - '0'; });

    return v;
}

std::vector<std::vector<int>> series::slice(std::string_view s, size_t len) {
    if (len > s.length()) {
        throw std::domain_error("invalid input");
    }

    std::vector<std::vector<int>> v;

    for (size_t i = 0; i <= s.length() - len; i++) {
        std::string_view sv = s.substr(i, len);
        v.push_back(digits(sv));
    }

    return v;
}
