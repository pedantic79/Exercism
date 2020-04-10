#include "atbash_cipher.h"
#include <algorithm>

char tolower(char ch) {
    return static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
}

char transform(char c) {
    if (std::isalpha(c)) {
        return static_cast<char>('a' + 'z' - tolower(c));
    } else {
        return c;
    }
}

std::string atbash_cipher::encode(std::string_view s) {
    int count = 0;
    std::string output;

    std::for_each(s.begin(), s.end(), [&](char c) {
        if (std::isalnum(c)) {
            if (count != 0 && count % 5 == 0) {
                output.push_back(' ');
            }

            count++;
            output.push_back(transform(c));
        }
    });

    return output;
}

std::string atbash_cipher::decode(std::string_view s) {
    std::string output;

    std::for_each(s.begin(), s.end(), [&](char c) {
        if (c != ' ') {
            output.push_back(transform(c));
        }
    });

    return output;
}
