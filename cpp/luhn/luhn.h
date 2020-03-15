#if !defined(LUHN_H)
#define LUHN_H
#include <string>

namespace luhn {
bool valid(std::string_view);
} // namespace luhn

#endif // LUHN_H
