#include "hamming.h"
#include <algorithm>

namespace hamming {
    // const std::string& instead of std::string_view for C++11 and 14
    int compute(std::string_view left, std::string_view right) {
        if (left.size() != right.size()) {
            throw std::domain_error("size mismatch");
        }

        using ch = decltype(right)::value_type;
        auto itr = std::begin(right);
        return std::count_if(std::begin(left), std::end(left),
            [&itr](const ch& c) { return c != *itr++; }
        );
    }
}
