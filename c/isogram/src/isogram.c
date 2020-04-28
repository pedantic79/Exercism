#include "isogram.h"
#include <stddef.h>
#include <stdint.h>

static int ascii_distance(char c) {
    int offset = -1;

    if (c >= 'A' && c <= 'Z') {
        offset = c - 'A';
    } else if (c >= 'a' && c <= 'z') {
        offset = c - 'a';
    }

    return offset;
}

bool is_isogram(const char phrase[]) {
    if (phrase == NULL) {
        return false;
    }

    uint32_t set = 0;

    for (const char *c = phrase; *c != '\0'; c++) {
        int offset = ascii_distance(*c);

        if (offset >= 0) {
            uint32_t mask = 1 << offset;
            if (set & mask) {
                return false;
            } else {
                set |= mask;
            }
        }
    }

    return true;
}
