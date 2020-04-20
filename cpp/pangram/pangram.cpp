#include "pangram.h"

#include <algorithm>
#include <bitset>
#include <cctype>
#include <string>

const std::string alpha = "QWERTYUIOPASDFGHJKLZXCVBNM";

bool pangram::is_pangram(const std::string &str) {
    std::bitset<26> letters;

    std::for_each(str.cbegin(), str.cend(), [&](auto c) {
        if (std::isalpha(c)) {
            const auto pos = std::tolower(c) - 'a';
            letters.set(pos);
        }
    });

    return letters.all();
}
