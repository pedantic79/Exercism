#if !defined(ROBOT_NAME_H)
#define ROBOT_NAME_H
#include <string>
#include <unordered_set>

namespace robot_name {
class robot {
    std::string name_;
    static std::unordered_set<std::string> namelist_;
    void generateRandomName();

  public:
    robot();

    // Don't allow the robot to be copied
    robot(const robot &) = delete;
    robot &operator=(const robot &) = delete;

    // Rule of 5, allow the robot to be moved and deleted
    robot(robot &&) = default;
    robot &operator=(robot &&) = default;
    ~robot() = default;

    void reset();
    const std::string name() const;
};
} // namespace robot_name

#endif // ROBOT_NAME_H
