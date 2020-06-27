#include "anagram.h"
#include <algorithm>
#include <locale>
#include <string>

namespace anagram {

std::string anagram::lowercase(const std::string &s) const {
    std::string v;
    std::transform(s.begin(), s.end(), std::back_inserter(v), [](char c) {
        return std::tolower(c, std::locale::classic());
    });
    return v;
}

std::string anagram::sort(const std::string &input) const {
    std::string v{input};
    std::sort(v.begin(), v.end());
    return v;
}

anagram::anagram(const std::string &s)
    : lowercase_{lowercase(s)}, fingerprint_{sort(lowercase_)} {}

std::vector<std::string> anagram::matches(const std::vector<std::string> &v) {
    std::vector<std::string> result;
    std::copy_if(v.begin(), v.end(), std::back_inserter(result),
                 [&](const std::string &s) {
                     auto lc = lowercase(s);
                     return lc != lowercase_ && sort(lc) == fingerprint_;
                 });

    return result;
}

} // namespace anagram
