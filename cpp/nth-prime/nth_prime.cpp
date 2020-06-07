#include "nth_prime.h"
#include <algorithm>
#include <numeric>
#include <stdexcept>
#include <vector>

namespace {
bool check_prime(const std::vector<unsigned> &primes, unsigned number) {
    for (auto p : primes) {
        if (p * p > number) {
            return true;
        }

        if (number % p == 0) {
            return false;
        }
    }

    return true;
}

template <typename T, std::size_t N> constexpr T prod(T (&array)[N]) {
    typename std::remove_const<T>::type p = 1;
    for (std::size_t i = 0; i < N; i++) {
        p *= array[i];
    }
    return p;
}
} // namespace

unsigned nth_prime::nth(unsigned number) {
    if (number == 0) {
        throw std::domain_error{"invalid input"};
    }

    // [c | c <- [1..product(spokes)], all ((0 /=) . (c `mod`)) spokes]
    constexpr int SPOKES[]{2, 3, 5, 7};
    constexpr int R[]{1, 11, 13, 191, 193, 197, 199, 209};

    // constexpr int SPOKES[]{2, 3, 5};
    // constexpr int R[]{1, 7, 11, 13, 17, 19, 23, 29};

    // constexpr int SPOKES[]{2, 3};
    // constexpr int R[]{1, 5};

    constexpr int K_MULT{prod(SPOKES)};
    std::vector<unsigned> primes{std::begin(SPOKES), std::end(SPOKES)};

    for (unsigned k = 0; primes.size() <= number; k++) {
        // Use Pritchard's Prime Wheel Sieve with 2, 3 spokes
        // This produces prime candidates in the form 6k + 1 and 6k + 5
        // This is equivalent of skipping all multiples of 2 and 3. Besides,
        // 1; 5 is the only number less than 6 that is coprime to 2 and 3.
        //
        // Using more spokes would be more efficient, but would require to
        // hardcode in more coprime values. For example, a 2,3,5 spoke wheel
        // would have candidates of 30k+{1,7,11,13,17,19,23,29}
        //
        // A basic odd only Sieve of Eratosthenes (i.e. 2 and every odd
        // number larger than 3) is equivalent to a Prime Wheel with just a
        // 2 spoke. It's candidates are 2k+1
        //
        // Also, we need to remove k=0, r=1, which is the case where the
        // candidate is 1, which we don't consider a prime
        for (unsigned r : R) {
            unsigned cand = K_MULT * k + r;

            if (cand > 1 && check_prime(primes, cand)) {
                primes.push_back(cand);
                if (primes.size() == number) {
                    break;
                }
            }
        }
    }

    return primes[number - 1];
}
