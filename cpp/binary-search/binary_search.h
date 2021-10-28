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
    size_t right = haystack.size();

    while (left < right) {
        const size_t mid = left + (right - left) / 2;

        if (needle == haystack[mid])
            return mid;

        if (needle < haystack[mid]) {
            right = mid;
        } else {
            left = mid + 1;
        }
    }

    throw std::domain_error("value not found");
}
} // namespace binary_search

#endif // BINARY_SEARCH_H
