#include "difference_of_squares.h"

namespace difference_of_squares {

unsigned square_of_sum(unsigned number) {
    const auto s = number * (number + 1) / 2;
    return s * s;
}

unsigned sum_of_squares(unsigned number) {
    return number * (number + 1) * (2 * number + 1) / 6;
}

unsigned difference(unsigned number) {
    return square_of_sum(number) - sum_of_squares(number);
}

} // namespace difference_of_squares
