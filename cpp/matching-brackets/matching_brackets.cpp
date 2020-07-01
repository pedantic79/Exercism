#include "matching_brackets.h"
#include <iostream>
#include <stack>
#include <string>

namespace {
char get_match(char c) {
    switch (c) {
    case '(':
        return ')';
    case '[':
        return ']';
    case '{':
        return '}';
    }

    return c;
}
} // namespace

bool matching_brackets::check(const std::string &s) {
    std::stack<char> check;

    for (char c : s) {
        switch (c) {
        case '(':
        case '[':
        case '{':
            check.push(get_match(c));
            break;
        case ')':
        case ']':
        case '}':
            if (!check.empty() && check.top() == c) {
                check.pop();
            } else {
                return false;
            }
        }
    }

    return check.empty();
}
