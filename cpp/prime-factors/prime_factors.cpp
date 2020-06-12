#include "prime_factors.h"

std::vector<int> prime_factors::of(int n) {
    int divisor = 2;
    std::vector<int> v;

    while (n > 1) {
        if (n % divisor == 0) {
            v.push_back(divisor);
            n /= divisor;
        } else {
            divisor += divisor > 2 ? 2 : 1;
        }
    }

    return v;
}
