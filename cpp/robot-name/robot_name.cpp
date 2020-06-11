#include "robot_name.h"
#include <random>
#include <string>
#include <unordered_map>

using std::mt19937;
using std::random_device;
using std::string;
using std::uniform_int_distribution;
using std::unordered_set;

unordered_set<string> robot_name::robot::namelist_;

robot_name::robot::robot() {
    name_.reserve(5);
    reset();
}

void robot_name::robot::reset() {
    do {
        generateRandomName();
    } while (namelist_.find(name_) != namelist_.end());

    namelist_.insert(name_);
}

const string robot_name::robot::name() const { return name_; }

void robot_name::robot::generateRandomName() {
    thread_local random_device rd;
    thread_local mt19937 random_generator{rd()};

    uniform_int_distribution<char> random_ascii_letter{'A', 'Z'};
    uniform_int_distribution<char> random_ascii_number{'0', '9'};

    name_.erase();
    for (int i = 0; i < 2; ++i) {
        name_ += random_ascii_letter(random_generator);
    }
    for (int i = 0; i < 3; ++i) {
        name_ += random_ascii_number(random_generator);
    }
}
