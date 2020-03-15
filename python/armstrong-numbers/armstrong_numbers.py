def is_armstrong_number(number):
    length = len(str(number))
    total = sum([int(d) ** length for d in str(number)])

    return total == number
