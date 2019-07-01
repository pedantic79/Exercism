def is_armstrong_number(number):
    l = len(str(number))
    total = sum([int(d) ** l for d in str(number)])

    return total == number
