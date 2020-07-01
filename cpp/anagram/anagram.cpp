#include "anagram.h"
#include <algorithm>
#include <initializer_list>
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

std::string anagram::sort(std::string &&input) const {
    std::string s{input};
    std::sort(s.begin(), s.end());
    return s;
}

anagram::anagram(const std::string &s)
    : lowercase_{lowercase(s)}, freq_{sort(std::string{lowercase_})} {}

std::vector<std::string>
anagram::matches(std::initializer_list<std::string> v) {
    std::vector<std::string> result;
    std::copy_if(v.begin(), v.end(), std::back_inserter(result),
                 [&](const std::string &s) {
                     auto lc = lowercase(s);
                     return lc != lowercase_ && sort(std::move(lc)) == freq_;
                 });

    return result;
}

} // namespace anagram
