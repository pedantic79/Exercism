#include "crypto_square.h"
#include <algorithm>
#include <cmath>
#include <locale>
#include <sstream>
#include <vector>

namespace {

// A combination between transform and copy_if
// See: https://en.cppreference.com/w/cpp/algorithm/transform
// See: https://en.cppreference.com/w/cpp/algorithm/copy
template <class InputIt, class OutputIt, class Predicate, class Operation>
void transform_if(InputIt first, InputIt last, OutputIt dest, Predicate pred,
                  Operation op) {
    while (first != last) {
        if (pred(*first))
            *dest++ = op(*first);

        ++first;
    }
}
} // namespace

namespace crypto_square {
cipher::cipher(const std::string &s) {
    transform_if(
        s.begin(), s.end(), std::back_inserter(plain_),
        [](char c) { return std::isalnum(c, std::locale::classic()); },
        [](char c) { return std::tolower(c, std::locale::classic()); });
}

std::vector<std::string> cipher::plain_text_segments() const {
    auto width = static_cast<ptrdiff_t>(
        std::ceil(std::sqrt(static_cast<double>(plain_.length()))));
    std::vector<std::string> result;

    for (auto itr = plain_.begin(); std::distance(itr, plain_.end()) > 0;
         itr += width) {
        auto dist = std::min(width, std::distance(itr, plain_.end()));
        result.emplace_back(itr, itr + dist);
    }

    return result;
}

std::string cipher::normalized_cipher_text(bool normalize) const {
    if (plain_.empty()) {
        return {};
    }

    auto block = plain_text_segments();

    std::stringstream oss;
    for (size_t col = 0; col < block[0].length(); col++) {
        if (normalize && col != 0) {
            oss << ' ';
        }

        for (size_t row = 0; row < block.size(); row++) {
            if (col < block[row].length()) {
                oss << block[row][col];
            } else if (normalize) {
                oss << ' ';
            }
        }
    }

    return oss.str();
}

std::string cipher::cipher_text() const {
    return normalized_cipher_text(false);
}
} // namespace crypto_square
