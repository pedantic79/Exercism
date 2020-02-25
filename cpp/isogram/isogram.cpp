#include "isogram.h"
#include <string>
#include <unordered_set>

bool isogram::is_isogram(std::string_view str) {
    std::unordered_set<char> exists;

    for (auto ch : str) {
        if (ch != '-' && ch != ' ' && !exists.insert(std::tolower(ch)).second) {
            return false;
        }
    }

    return true;
}
