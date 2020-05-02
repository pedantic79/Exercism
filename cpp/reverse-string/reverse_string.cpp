#include "reverse_string.h"
#include "test/catch.hpp"
#include <algorithm>
#include <codecvt>
#include <locale>
#include <string>
#include <string_view>

std::string to_utf8(std::u32string_view s) {
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> conv;
    return conv.to_bytes(s.begin(), s.end());
}

std::u32string to_utf32(std::string_view s) {
    std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t> conv;
    return conv.from_bytes(s.begin(), s.end());
}

std::string reverse_string::reverse_string(std::string_view s) {
    auto unicode = to_utf32(s);

    std::reverse(unicode.begin(), unicode.end());
    return to_utf8(unicode);
}

TEST_CASE("wide_character") {
    REQUIRE("猫子" == reverse_string::reverse_string("子猫"));
}
