#include "square_root.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

uint16_t squareRoot(uint16_t radicand) {
    // See: https://en.wikipedia.org/wiki/Integer_square_root
    uint16_t result = 0;
    uint16_t bit = 1 << 14;

    while (bit > radicand) {
        bit >>= 2;
    }

    for (; bit != 0; bit >>= 2) {
        if (radicand >= result + bit) {
            radicand -= result + bit;
            result = (result >> 1) + bit;
        } else {
            result >>= 1;
        }
    }

    return result;
}
