#if !defined(ANAGRAM_H)
#define ANAGRAM_H
#include <string>
#include <vector>

namespace anagram {

class anagram {
    std::string lowercase_;
    std::string fingerprint_;
    std::string lowercase(const std::string &) const;
    std::string sort(const std::string &) const;

  public:
    explicit anagram(const std::string &);
    std::vector<std::string> matches(const std::vector<std::string> &);
};

} // namespace anagram

#endif // ANAGRAM_H
