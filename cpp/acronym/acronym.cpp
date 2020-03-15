#include "acronym.h"
#include <locale>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

struct dash_is_ws : std::ctype<char> {
    dash_is_ws() : std::ctype<char>(make_table()) {}
    static const mask *make_table() {
        static std::vector<mask> v(classic_table(),
                                   classic_table() + table_size);
        v['-'] |= space;
        return &v[0];
    }
};

string acronym::acronym(const string &s) {
    ostringstream oss;
    istringstream iss(s);
    iss.imbue(locale(iss.getloc(), make_unique<dash_is_ws>().release()));

    string word;
    while (iss >> word) {
        oss << toupper(word[0], locale(oss.getloc()));
    }

    return oss.str();
}
