#include "rna_transcription.h"
#include <algorithm>
#include <numeric>

using namespace std;

char rna_transcription::to_rna(char nucleotide) {
    switch (nucleotide) {
    case 'C':
        return 'G';
    case 'G':
        return 'C';
    case 'A':
        return 'U';
    case 'T':
        return 'A';

    default:
        return nucleotide;
    }
}

string rna_transcription::to_rna(string_view rna) {
    return accumulate(next(begin(rna)), end(rna),
                      string(1, to_rna(rna.front())),
                      [](string accum, auto nucleotide) {
                          return move(accum + to_rna(nucleotide));
                      });
}
