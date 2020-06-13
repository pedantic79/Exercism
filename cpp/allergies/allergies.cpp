#include "allergies.h"
#include <string>
#include <unordered_map>
#include <unordered_set>

allergies::allergy_test::allergy_test(int score) {
    static const std::unordered_map<int, std::string> allergen_map{
        {1, "eggs"},         {2, "peanuts"},   {4, "shellfish"},
        {8, "strawberries"}, {16, "tomatoes"}, {32, "chocolate"},
        {64, "pollen"},      {128, "cats"}};

    for (const auto &entry : allergen_map) {
        if ((entry.first & score) == entry.first) {
            allergies_.insert(entry.second);
        }
    }
}

bool allergies::allergy_test::is_allergic_to(const char *s) const {
    return allergies_.find(s) != allergies_.cend();
}

std::unordered_set<std::string> allergies::allergy_test::get_allergies() const {
    return allergies_;
}
