#include "queen_attack.h"

#include <algorithm>
#include <array>
#include <iterator>
#include <sstream>
#include <stdexcept>

namespace {
constexpr int SIZE = 8;
}

queen_attack::chess_board::chess_board(const std::pair<int, int> &white,
                                       const std::pair<int, int> &black)
    : white_(white), black_(black) {
    if (white == black) {
        throw std::domain_error("queen positions must be unique");
    }
}

std::pair<int, int> queen_attack::chess_board::white() const { return white_; }
std::pair<int, int> queen_attack::chess_board::black() const { return black_; }

bool queen_attack::chess_board::can_attack() const {
    auto row = white_.first == black_.first;
    auto col = white_.second == black_.second;

    auto pos_diag =
        white_.first - black_.first == white_.second - black_.second;

    auto neg_diag =
        white_.first + black_.first == white_.second + black_.second;

    return row || col || pos_diag || neg_diag;
}

queen_attack::chess_board::operator std::string() const {
    std::stringstream output;
    std::array<std::array<char, SIZE>, SIZE> board;
    std::array<char, SIZE> empty_row;
    empty_row.fill('_');
    board.fill(empty_row);

    board.at(white_.first).at(white_.second) = 'W';
    board.at(black_.first).at(black_.second) = 'B';

    // space seperate each row
    for (const auto &row : board) {
        std::copy(row.begin(), row.end() - 1,
                  std::ostream_iterator<char>(output, " "));

        output << row.back() << '\n';
    }

    return output.str();
}
