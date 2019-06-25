#include "pangram.h"

#include <algorithm>
#include <bitset>
#include <cctype>

using namespace std;

bool pangram::is_pangram(string_view str) {
    std::bitset<26> letters;

    for_each(begin(str), end(str), [&](auto c) {
        if (isalpha(c)) {
            const auto pos = tolower(c) - 'a';
            letters.set(pos);
        }
    });

    return letters.all();
}
