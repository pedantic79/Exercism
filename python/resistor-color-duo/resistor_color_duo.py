def value(colors):
    return int(''.join([str(RESISTORS.index(c)) for c in colors]))


RESISTORS = [
    'black',
    'brown',
    'red',
    'orange',
    'yellow',
    'green',
    'blue',
    'violet',
    'grey',
    'white'
]
