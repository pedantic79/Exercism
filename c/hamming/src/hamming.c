#include "hamming.h"
#include <stddef.h>

int compute(const char *lhs, const char *rhs) {
    if (lhs == NULL || rhs == NULL) {
        return -1;
    }

    int count = 0;
    while (*lhs != '\0' && *rhs != '\0') {
        if (*lhs != *rhs) {
            count++;
        }

        lhs++;
        rhs++;
    }

    if (*lhs != *rhs) {
        return -1;
    }

    return count;
}
