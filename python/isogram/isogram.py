def is_isogram(string):
    existing = set()

    for c in string:
        lc = c.lower()
        if lc.isalpha():
            if lc in existing:
                return False
            else:
                existing.add(lc)

    return True
