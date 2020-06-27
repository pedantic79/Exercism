#include "pangram.h"
#include <bitset>
#include <locale>
#include <string>

bool pangram::is_pangram(const std::string &str) {
    std::bitset<26> letters;

    for (auto c : str) {
        if (std::isalpha(c, std::locale::classic())) {
            const auto pos = std::tolower(c, std::locale::classic()) - 'a';
            letters.set(pos);
        }
    }

    return letters.all();
}
