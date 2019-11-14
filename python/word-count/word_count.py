import re
import collections

# Any non-(alphanumeric or single-quote character)
RE_SPLITTER = re.compile("[^a-zA-Z0-9']")


def count_words(sentence):
    return collections.Counter(word.lower().strip('\'"') for word in RE_SPLITTER.split(sentence) if word)
