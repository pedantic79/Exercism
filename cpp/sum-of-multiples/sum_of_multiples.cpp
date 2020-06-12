#include "sum_of_multiples.h"

#include <algorithm>
#include <vector>

int sum_of_multiples::to(const std::vector<int> &factors, int limit) {
    int sum = 0;
    for (int i = 0; i < limit; i++) {
        if (std::any_of(factors.cbegin(), factors.cend(),
                        [&i](const int f) { return f > 0 && i % f == 0; })) {
            sum += i;
        }
    }

    return sum;
}
