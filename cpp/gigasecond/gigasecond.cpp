#include "gigasecond.h"

using namespace boost::posix_time;

ptime gigasecond::advance(const ptime &t) {
    // quote digit seperator added in C++14
    return t + seconds(1'000'000'000);
}
