#include <algorithm>
#include <cctype>
#include <map>
#include <numeric>
#include <string>
#include <vector>

namespace scrabble_score {

const std::map<std::string, int> scrabble{
    {"aeioulnrst", 1}, {"dg", 2}, {"bcmp", 3}, {"fhvwy", 4},
    {"k", 5},          {"jx", 8}, {"qz", 10}};

int score_char(char ch) {
    char c = std::tolower(ch);
    auto i =
        std::find_if(std::begin(scrabble), std::end(scrabble), [c](auto p) {
            return p.first.find(c) != std::string::npos;
        });

    if (i == std::end(scrabble)) {
        return -1; // TODO: Not found, how do we handle this?
    } else {
        return i->second;
    }
}

int score(std::string_view str) {
    std::vector<int> v(str.length());

    std::transform(std::begin(str), std::end(str),
                   std::back_insert_iterator<decltype(v)>(v), score_char);

    return std::accumulate(std::begin(v), std::end(v), 0);
}

} // namespace scrabble_score
