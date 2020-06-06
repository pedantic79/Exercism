#include "pangram.h"
#include <bitset>
#include <string>

bool pangram::is_pangram(const std::string &str) {
    std::bitset<26> letters;

    for (auto c : str) {
        if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
            const auto pos = (c | 0x20) - 'a';
            letters.set(pos);
        }
    }

    return letters.all();
}
