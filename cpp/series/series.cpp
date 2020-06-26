#include "series.h"
#include <algorithm>
#include <stdexcept>

std::vector<int> series::digits(const std::string &s) {
    std::vector<int> v;

    std::transform(s.begin(), s.end(), std::back_inserter(v),
                   [](auto c) { return c - '0'; });

    return v;
}

// This would be greatly improved by using a string_view which is available in
// C++17. Using a string_view allow us to avoid extra allocations which aren't
// necessary, and quite a bit more efficient. Change everywhere that is a
// const std::string & to string_view
std::vector<std::vector<int>> series::slice(const std::string &s, size_t len) {
    if (len > s.length()) {
        throw std::domain_error("invalid input");
    }

    std::vector<std::vector<int>> v;

    for (size_t i = 0; i <= s.length() - len; i++) {
        auto str = s.substr(i, len);
        v.push_back(digits(str));
    }

    return v;
}
