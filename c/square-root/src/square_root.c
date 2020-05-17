#include "square_root.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

uint16_t squareRoot(uint16_t radicand) {
    uint16_t i = 0;

    while (i * i <= radicand)
        i++;

    return i - 1;
}
