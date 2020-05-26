#ifndef NUCLEOTIDE_COUNT_H
#define NUCLEOTIDE_COUNT_H
#include <algorithm>
#include <map>
#include <string>

namespace nucleotide_count {
class counter {
    std::string s_;

  public:
    explicit counter(const std::string &dna_sequence);
    int count(char nucleotide) const;
    std::map<char, int> nucleotide_counts() const;
};

} // namespace nucleotide_count
#endif
