#include "reverse_string.h"

using namespace std;

// Using string_view from C++17, if this were to be C++11 compliant I would
// use a `const string&` instead. This however means that if a `const char *`
// would create a string object implicitly. The only work around to this would
// be to have two implementations one that took a `const string&` and one
// that took a `const char *`
string reverse_string::reverse_string(string_view s) {
    return string(rbegin(s), rend(s));
}
