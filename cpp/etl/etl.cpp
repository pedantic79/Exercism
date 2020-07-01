#include "etl.h"
#include <locale>
#include <map>
#include <vector>

std::map<char, int> etl::transform(std::map<int, std::vector<char>> input) {
    std::map<char, int> result;

    for (auto entry : input) {
        auto count = entry.first;

        for (auto letter : entry.second) {
            auto lc = std::tolower(letter, std::locale::classic());
            auto pos = result.find(lc);

            if (pos != result.end()) {
                pos->second += count;
            } else {
                result.insert(std::make_pair(lc, count));
            }
        }
    }

    return result;
}
