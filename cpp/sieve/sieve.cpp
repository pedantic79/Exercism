#include "sieve.h"
#include <vector>

std::vector<int> sieve::primes(size_t num) {
    std::vector<bool> sieve(num + 1, true);

    for (size_t i = 2; i < sieve.size(); i++) {
        if (sieve[i]) {
            for (size_t j = i * 2; j < sieve.size(); j += i) {
                sieve[j] = false;
            }
        }
    }

    std::vector<int> output;
    for (size_t i = 2; i < sieve.size(); i++) {
        if (sieve[i]) {
            output.push_back(i);
        }
    }

    return output;
}
