import re
import collections

# Any non-(alphanumeric or single-quote character)
RE_SPLITTER = re.compile("[^a-zA-Z0-9']")


def counter(iterator):
    count = collections.defaultdict(int)
    for entry in iterator:
        count[entry] += 1

    return count


def count_words(sentence):
    return counter(
        word.lower().strip("'\"")
        for word in RE_SPLITTER.split(sentence)
        if word
    )
