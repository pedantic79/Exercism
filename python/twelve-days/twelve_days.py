ORDINALS = [
    "first",
    "second",
    "third",
    "fourth",
    "fifth",
    "sixth",
    "seventh",
    "eighth",
    "ninth",
    "tenth",
    "eleventh",
    "twelfth"
]

PRESENTS = [
    'a Partridge in a Pear Tree',
    'two Turtle Doves',
    'three French Hens',
    'four Calling Birds',
    'five Gold Rings',
    'six Geese-a-Laying',
    'seven Swans-a-Swimming',
    'eight Maids-a-Milking',
    'nine Ladies Dancing',
    'ten Lords-a-Leaping',
    'eleven Pipers Piping',
    'twelve Drummers Drumming'
]


def generate_day(day):
    for d in range(day, 0, -1):
        yield f'{PRESENTS[d]},'

    if day > 0:
        yield f'and {PRESENTS[0]}.'
    else:
        yield f'{PRESENTS[0]}.'


def verse(day):
    return ' '.join([
        f'On the {ORDINALS[day]} day of Christmas my true love gave to me:',
        *generate_day(day)
    ])


def recite(start_verse, end_verse):
    return [verse(day) for day in range(start_verse - 1, end_verse)]
