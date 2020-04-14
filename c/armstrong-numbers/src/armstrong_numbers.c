#include "armstrong_numbers.h"

int num_length(int num) {
    int count = 0;

    while (num > 0) {
        num /= 10;
        count++;
    }

    return count;
}

int ipow(int base, int exp) {
    int total = 1;

    for (int i = 0; i < exp; i++) {
        total *= base;
    }

    return total;
}

int is_armstrong_number(int candidate) {
    const int original_candidate = candidate;
    const int len = num_length(candidate);

    int armstrong_num = 0;

    while (candidate > 0) {
        armstrong_num += ipow(candidate % 10, len);
        candidate /= 10;
    }

    return armstrong_num == original_candidate;
}
