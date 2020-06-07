#include "robot_name.h"
#include <random>
#include <string>
#include <unordered_map>

namespace {
std::string generateRandomName() {
    std::string name;
    name.reserve(5);

    thread_local std::random_device rd;
    thread_local std::mt19937 random_generator{rd()};

    std::uniform_int_distribution<char> random_ascii_letter{'A', 'Z'};
    std::uniform_int_distribution<char> random_ascii_number{'0', '9'};

    for (int i = 0; i < 2; ++i) {
        name += random_ascii_letter(random_generator);
    }
    for (int i = 0; i < 3; ++i) {
        name += random_ascii_number(random_generator);
    }

    return name;
}
} // namespace

std::unordered_set<std::string> robot_name::robot::namelist_;

robot_name::robot::robot() { generateName(); }

void robot_name::robot::generateName() {
    do {
        name_ = generateRandomName();
    } while (namelist_.find(name_) != namelist_.end());

    namelist_.insert(name_);
}

void robot_name::robot::reset() { generateName(); }

const std::string robot_name::robot::name() const { return name_; }
