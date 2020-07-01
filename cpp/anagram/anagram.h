#if !defined(ANAGRAM_H)
#define ANAGRAM_H
#include <initializer_list>
#include <string>
#include <vector>

namespace anagram {

struct anagram {
    explicit anagram(const std::string &);
    std::vector<std::string> matches(std::initializer_list<std::string>);

  private:
    std::string lowercase_;
    std::string freq_;
    std::string lowercase(const std::string &) const;
    std::string sort(std::string &&) const;
};

} // namespace anagram

#endif // ANAGRAM_H
