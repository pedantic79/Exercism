#if !defined(BINARY_SEARCH_H)
#define BINARY_SEARCH_H
#include <stdexcept>
#include <vector>

namespace binary_search {
template <typename T>
size_t find(const std::vector<T> &haystack, const T &needle) {
    if (haystack.empty())
        throw std::domain_error("empty container");

    size_t left = 0;
    size_t right = haystack.size() - 1;

    while (left <= right) {
        const size_t mid = (left + right) / 2;

        if (needle == haystack[mid])
            return mid;

        if (needle < haystack[mid]) {
            // required to avoid underflow since we're using size_t
            // if we used something signed, then we could avoid it, as
            // right would be -1, and left would be 0.
            if (mid == 0)
                break;
            right = mid - 1;
        } else {
            left = mid + 1;
        }
    }

    throw std::domain_error("value not found");
}
} // namespace binary_search

#endif // BINARY_SEARCH_H
