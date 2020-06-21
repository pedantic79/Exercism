#include "binary_search.h"
#include <stdio.h>

int *binary_search(int value, const int *arr, size_t length) {
    if (length == 0) {
        return NULL;
    }

    size_t left = 0;
    size_t right = length - 1;

    while (left <= right) {
        size_t midpoint = (left + right) / 2;

        if (value == arr[midpoint]) {
            return (int *)(arr + midpoint);
        }

        if (value < arr[midpoint]) {
            /* this is required because we're using size_t, and this would
             * underflow. So check the range to avoid it.
             */
            if (midpoint == 0)
                break;
            right = midpoint - 1;
        } else {
            left = midpoint + 1;
        }
    }

    return NULL;
}
