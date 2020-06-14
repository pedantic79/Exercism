#include "protein_translation.h"
#include <string>
#include <unordered_map>
#include <vector>

std::vector<std::string> protein_translation::proteins(const std::string &s) {
    const static std::unordered_map<std::string, std::string> codon_to_protein =
        {{"AUG", "Methionine"},    {"UUU", "Phenylalanine"},
         {"UUC", "Phenylalanine"}, {"UUA", "Leucine"},
         {"UUG", "Leucine"},       {"UCU", "Serine"},
         {"UCC", "Serine"},        {"UCA", "Serine"},
         {"UCG", "Serine"},        {"UAU", "Tyrosine"},
         {"UAC", "Tyrosine"},      {"UGU", "Cysteine"},
         {"UGC", "Cysteine"},      {"UGG", "Tryptophan"},
         {"UAA", "STOP"},          {"UAG", "STOP"},
         {"UGA", "STOP"}};

    std::vector<std::string> v;
    for (size_t i = 0; i < s.length(); i += 3) {
        const auto &pos = codon_to_protein.find(s.substr(i, 3));
        if (pos != codon_to_protein.end()) {
            if (pos->second == "STOP") {
                break;
            }

            v.emplace_back(pos->second);
        }
    }

    return v;
}
