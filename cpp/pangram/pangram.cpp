#include "pangram.h"

#include <algorithm>
#include <string>
#include <cctype>

using namespace std;

bool pangram::is_pangram(string_view str) {
    const string letters = "abcdefghijklmnopqrstuvwxyz";

    return all_of(begin(letters), end(letters), [=](auto l) {
        const auto location = find_if(begin(str), end(str), [=](auto s) {
            return l == tolower(s);
        });
        return location != end(str);
    });
}