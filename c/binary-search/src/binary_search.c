#include "binary_search.h"
#include <stdio.h>

int *binary_search(int value, const int *arr, size_t length) {
    if (length == 0) {
        return NULL;
    }

    size_t left = 0;
    size_t right = length;

    while (left <= right) {
        size_t midpoint = (left + right) / 2;

        if (value == arr[midpoint]) {
            return (int *)(arr + midpoint);
        }

        if (midpoint == 0 || midpoint >= length) {
            break;
        } else if (value > arr[midpoint]) {
            left = midpoint + 1;
        } else {
            right = midpoint - 1;
        }
    }

    return NULL;
}
