#include "grains.h"

#include <cmath>
using namespace std;
uint64_t grains::square(uint8_t num) { return 1ULL << (num - 1); }

uint64_t grains::total() { return UINT64_MAX; }
