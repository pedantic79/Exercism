#if !defined(ROBOT_NAME_H)
#define ROBOT_NAME_H
#include <string>
#include <unordered_set>

namespace robot_name {
class robot {
    std::string name_;
    static std::unordered_set<std::string> namelist_;
    void generateName();

  public:
    robot();

    // Rule of 5
    robot(const robot &) = delete;
    robot(robot &&) = delete;
    void operator=(const robot &) = delete;
    void operator=(robot &&) = delete;
    ~robot() = default;

    void reset();
    const std::string name() const;
};
} // namespace robot_name

#endif // ROBOT_NAME_H
