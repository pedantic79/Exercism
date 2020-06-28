#include "crypto_square.h"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <iterator>
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
    std::string plain;

    transform_if(
        s.begin(), s.end(), std::back_inserter(plain),
        [](char c) { return std::isalnum(c, std::locale::classic()); },
        [](char c) { return std::tolower(c, std::locale::classic()); });

    auto width = static_cast<size_t>(
        std::ceil(std::sqrt(static_cast<double>(plain.length()))));

    for (size_t i = 0; i < plain.length(); i += width) {
        auto dist = std::min(width, plain.length() - i);
        square_.push_back(plain.substr(i, dist));
    }
}

std::string cipher::normalize_plain_text() const {
    std::stringstream oss;
    std::copy(square_.begin(), square_.end(),
              std::ostream_iterator<std::string>(oss));

    return oss.str();
}

std::vector<std::string> cipher::plain_text_segments() const { return square_; }

std::string cipher::normalized_cipher_text(bool normalize) const {
    if (square_.empty()) {
        return {};
    }

    std::stringstream oss;
    for (size_t col = 0; col < square_[0].length(); col++) {
        if (normalize && col != 0) {
            oss << ' ';
        }

        for (auto row : square_) {
            if (col < row.length()) {
                oss << row[col];
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
