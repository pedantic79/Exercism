#include "nucleotide_count.h"

namespace {
void is_valid(char c) {
    switch (c) {
    case 'A':
    case 'T':
    case 'C':
    case 'G':
        return;
    }

    throw std::invalid_argument{"invalid nucleotide"};
}

} // namespace

namespace nucleotide_count {

counter::counter(const std::string &dna_sequence) : s_{dna_sequence} {
    std::for_each(s_.begin(), s_.end(), ::is_valid);
}

int counter::count(char nucleotide) const {
    is_valid(nucleotide);

    return std::count_if(s_.begin(), s_.end(), [nucleotide](const char c) {
        return c == nucleotide;
    });
}

std::map<char, int> counter::nucleotide_counts() const {
    std::map<char, int> ret{{'A', 0}, {'T', 0}, {'C', 0}, {'G', 0}};

    for (char c : s_) {
        ret[c]++;
    }
    return ret;
}
} // namespace nucleotide_count
