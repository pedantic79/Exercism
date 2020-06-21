#if !defined(SERIES_H)
#define SERIES_H

#include <string_view>
#include <vector>

namespace series {
std::vector<int> digits(std::string_view s);
std::vector<std::vector<int>> slice(std::string_view s, size_t len);
} // namespace series

#endif // SERIES_H
