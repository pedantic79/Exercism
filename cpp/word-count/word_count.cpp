#include "word_count.h"
#include <locale>
#include <map>
#include <sstream>
#include <string>
#include <vector>

struct punct_is_space : std::ctype<char> {
    punct_is_space() : std::ctype<char>(make_table()) {}
    static const mask *make_table() {
        static std::vector<mask> v(classic_table(),
                                   classic_table() + table_size);

        // Set punctuation characters as whitespace
        for (auto c : "&@$!%^&.,:") {
            v[c] |= space;
        }

        return &v[0];
    }
};

bool surrounded_by_quotes(const std::string &s) {
    return s[0] == '\'' && s[s.length() - 1] == '\'';
}

std::map<std::string, int> word_count::words(const std::string &s) {
    std::istringstream iss(s);
    iss.imbue(std::locale(iss.getloc(),
                          std::make_unique<punct_is_space>().release()));

    std::map<std::string, int> result;
    std::string word;
    while (iss >> word) {
        std::transform(word.begin(), word.end(), word.begin(), [](char c) {
            return std::tolower(c, std::locale::classic());
        });

        if (surrounded_by_quotes(word)) {
            word = word.substr(1, word.length() - 2);
        }

        result[word] += 1;
    }

    return result;
}
