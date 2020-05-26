#include "grade_school.h"
#include <algorithm>
#include <map>
#include <string>
#include <vector>

namespace grade_school {

void school::add(const std::string &name, int grade) {
    students_[grade].push_back(name);
    std::sort(students_.at(grade).begin(), students_.at(grade).end());
}

std::map<int, std::vector<std::string>> school::roster() const {
    return students_;
}

std::vector<std::string> school::grade(int gradeNum) const {
    auto pos = students_.find(gradeNum);

    return (pos != students_.end()) ? pos->second : std::vector<std::string>{};
}

} // namespace grade_school
