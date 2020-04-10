#ifndef NUCLEOTIDE_COUNT_H
#define NUCLEOTIDE_COUNT_H
#include <map>
#include <string>
#include <algorithm>

namespace nucleotide_count {
class counter {
    std::string s;

    static void is_valid(char c) {
        switch (c) {
        case 'A':
        case 'T':
        case 'C':
        case 'G':
            return;
        }

        throw std::invalid_argument{"invalid nucleotide"};
    }

  public:
    counter(const std::string &dna_sequence) : s{dna_sequence} {
        std::for_each(std::begin(s), std::end(s), is_valid);
    }

    int count(char nucleotide) const {
        is_valid(nucleotide);

        return std::count_if(
            std::begin(s), std::end(s),
            [nucleotide](const char c) { return c == nucleotide; });
    }

    std::map<char, int> nucleotide_counts() const {
        std::map<char, int> ret{{'A', 0}, {'T', 0}, {'C', 0}, {'G', 0}};

        for (char c : s) {
            ret[c]++;
        }
        return ret;
    }
};

} // namespace nucleotide_count
#endif
