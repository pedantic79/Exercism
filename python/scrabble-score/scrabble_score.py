STRING_TO_SCORES = {
    "AEIOULNRST": 1,
    "DG": 2,
    "BCMP": 3,
    "FHVWY": 4,
    "K": 5,
    "JX": 8,
    "QZ": 10,
}

LETTER_VALUES = {
    letter: score
    for (string, score) in STRING_TO_SCORES.items()
    for letter in string
}


def score(word):
    return sum(LETTER_VALUES[character] for character in word.upper())
