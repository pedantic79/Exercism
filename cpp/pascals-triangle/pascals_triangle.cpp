#include "pascals_triangle.h"
#include <vector>

using namespace std;

vector<int> row(const vector<int> &last_row) {
    vector<int> v;

    if (last_row.size() == 0) {
        v.push_back(1);
    } else {
        v.push_back(1);
        if (last_row.size() > 1) {
            for (auto itr = last_row.begin(); itr != last_row.end() - 1;
                 itr++) {
                v.push_back(*itr + *(itr + 1));
            }
        }

        v.push_back(1);
    }

    return v;
}

vector<vector<int>> pascals_triangle::generate_rows(int row_count) {
    vector<vector<int>> triangle;

    if (row_count > 0) {
        vector<int> blank;
        triangle.emplace_back(row(blank));

        for (int i = 1; i < row_count; i++) {
            triangle.emplace_back(row(triangle[i - 1]));
        }
    }

    return triangle;
}
