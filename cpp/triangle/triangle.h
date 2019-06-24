#ifndef TRIANGLE_H
#define TRIANGLE_H

#include <algorithm>
#include <array>
#include <unordered_set>

namespace triangle {
enum flavor { equilateral, isosceles, scalene };

template <class T> flavor kind(const T &a, const T &b, const T &c) {
    std::array<T, 3> sides = {a, b, c};
    std::sort(std::begin(sides), std::end(sides));

    if (sides[0] + sides[1] <= sides[2] ||
        std::any_of(std::begin(sides), std::end(sides),
                    [](const T &i) { return i == T(); })) {
        throw std::domain_error("Invalid triangle");
    }

    std::unordered_set<T> uniq_sides = {a, b, c};
    if (uniq_sides.size() == 1) {
        return flavor::equilateral;
    } else if (uniq_sides.size() == 2) {
        return flavor::isosceles;
    } else {
        return flavor::scalene;
    }
}

} // namespace triangle

#endif
