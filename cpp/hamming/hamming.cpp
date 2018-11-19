#include "hamming.h"

namespace hamming {
    // const std::string& instead of std::string_view for C++11 and 14
    int compute(std::string_view left, std::string_view right) {
        if (left.size() != right.size()) {
            throw std::domain_error("size mismatch");
        }

        int count = 0;
        auto itr = std::begin(right);
        for (const auto& c : left) {
            if (*itr != c) {
                count++;
            }
            itr++;
        }
        return count;
    }
}
