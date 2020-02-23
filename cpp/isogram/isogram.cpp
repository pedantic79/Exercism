#include "isogram.h"
#include <string>
#include <unordered_set>

bool isogram::is_isogram(std::string_view str) {
    std::unordered_set<char> exists;

    for (auto ch : str) {
        if (ch == '-' || ch == ' ') {
            continue;
        }

        auto lower = std::tolower(ch);
        // In C++20, we should use exists.contains() which returns a bool
        if (exists.count(lower) == 1) {
            return false;
        } else {
            exists.insert(lower);
        }
    }

    return true;
}
