def convert(number):
    s = ''.join(sound for (denom, sound) in
                [(3, "Pling"), (5, "Plang"), (7, "Plong")] if number % denom == 0)

    return s if s else str(number)
