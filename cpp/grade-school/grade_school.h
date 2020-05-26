#if !defined(GRADE_SCHOOL_H)
#define GRADE_SCHOOL_H
#include <map>
#include <string>
#include <vector>

namespace grade_school {

class school {
    std::map<int, std::vector<std::string>> students_;

  public:
    void add(const std::string &, int);
    std::map<int, std::vector<std::string>> roster() const;
    std::vector<std::string> grade(int) const;
};

} // namespace grade_school

#endif // GRADE_SCHOOL_H
