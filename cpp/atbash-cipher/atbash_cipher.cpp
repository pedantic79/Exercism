#include "atbash_cipher.h"
#include <iterator>
#include <locale>
#include <string>
#include <string_view>

namespace {
template <class InputIt, class OutputIt, class Pred, class Fct>
void transform_if(InputIt first, InputIt last, OutputIt dest, Pred predicate,
                  Fct transform) {
    while (first != last) {
        if (predicate(*first))
            *dest++ = transform(*first);

        ++first;
    }
}

char atbash_codec(char c) {
    if (std::isalpha(c, std::locale::classic())) {
        return static_cast<char>('a' + 'z' -
                                 tolower(c, std::locale::classic()));
    } else {
        return c;
    }
}
} // namespace

std::string atbash_cipher::encode(std::string_view s) {
    int count = 0;
    std::string output;

    for (auto c : s) {
        if (std::isalnum(c, std::locale::classic())) {
            if (count != 0 && count % 5 == 0) {
                output.push_back(' ');
            }

            count++;
            output.push_back(atbash_codec(c));
        }
    }

    return output;
}

std::string atbash_cipher::decode(std::string_view s) {
    std::string output;
    transform_if(
        s.begin(), s.end(), std::back_inserter(output),
        [](auto c) { return !std::isspace(c, std::locale::classic()); },
        atbash_codec);

    return output;
}
