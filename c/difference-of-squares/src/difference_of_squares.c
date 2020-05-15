#include "difference_of_squares.h"

unsigned int sum_of_squares(unsigned int number) {
    // See the partial sum formula
    // https://www.wolframalpha.com/input/?i=Sum%5Bn**2%5D
    return number * (number + 1) * (2 * number + 1) / 6;
}

unsigned int square_of_sum(unsigned int number) {
    // See the partial sum formula
    // https://www.wolframalpha.com/input/?i=Sum%5Bn%5D
    unsigned int value = number * (number + 1) >> 1;
    return value * value;
}

unsigned int difference_of_squares(unsigned int number) {
    return square_of_sum(number) - sum_of_squares(number);
}
