def is_pangram(sentence):
    letters = set(''.join(x for x in sentence.lower() if x.isalpha()))
    return len(letters) == 26
