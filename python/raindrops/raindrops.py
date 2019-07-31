def convert(number):
    s = ''.join(sound for (denom, sound) in
                [(3, "Pling"), (5, "Plang"), (7, "Plong")] if number % denom == 0)

    if not s:
        return str(number)
    else:
        return s
