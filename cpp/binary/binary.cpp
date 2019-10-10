#include "binary.h"

namespace binary {
int convert(std::string_view binary_number) {
    int result = 0;

    for (const auto &c : binary_number) {
        result <<= 1;
        if (c == '1') {
            result |= 1;
        } else if (c != '0') {
            return 0;
        }
    }
    return result;
}
} // namespace binary
