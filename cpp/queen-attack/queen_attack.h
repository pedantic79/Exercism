#if !defined(QUEEN_ATTACK_H)
#define QUEEN_ATTACK_H

#include <string>
#include <utility>

namespace queen_attack {
class chess_board {
    std::pair<int, int> white_;
    std::pair<int, int> black_;

  public:
    chess_board(const std::pair<int, int> & = {0, 3},
                const std::pair<int, int> & = {7, 3});
    std::pair<int, int> white() const;
    std::pair<int, int> black() const;
    bool can_attack() const;
    operator std::string() const;
};
} // namespace queen_attack

#endif // QUEEN_ATTACK_H
