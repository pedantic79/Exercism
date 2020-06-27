#include "crypto_square.h"
#include <algorithm>
#include <cmath>
#include <locale>
#include <sstream>
#include <utility>
#include <vector>

namespace {

// Like having transform https://en.cppreference.com/w/cpp/algorithm/transform
// and copy_if https://en.cppreference.com/w/cpp/algorithm/copy having a baby
template <class InputIt, class OutputIt, class Predicate, class Operation>
void transform_if(InputIt first, InputIt last, OutputIt dest, Predicate pred,
                  Operation op) {
    while (first != last) {
        if (pred(*first))
            *dest++ = op(*first);

        ++first;
    }
}

// pair<width, height>
std::pair<size_t, size_t> get_dimension(size_t length) {
    auto d = std::sqrt(static_cast<double>(length));
    return std::make_pair(static_cast<size_t>(std::ceil(d)),
                          static_cast<size_t>(d));
}

} // namespace

namespace crypto_square {
cipher::cipher(const std::string &s) {
    transform_if(
        s.begin(), s.end(), std::back_inserter(plain),
        [](char c) { return std::isalnum(c, std::locale::classic()); },
        [](char c) { return std::tolower(c, std::locale::classic()); });
}

std::vector<std::string> cipher::plain_text_segments() const {
    auto dim = get_dimension(plain.length());
    std::vector<std::string> result;

    for (size_t i = 0; i < plain.length(); i += dim.first) {
        auto dist = std::min(plain.length() - i, dim.first);
        result.emplace_back(plain.begin() + i, plain.begin() + i + dist);
    }

    return result;
}

std::string cipher::normalized_cipher_text(bool normalize) const {
    if (plain.empty()) {
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
