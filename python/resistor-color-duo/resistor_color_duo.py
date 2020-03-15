def value(colors):
    return int(''.join([str(RESISTORS.index(c)) for c in colors[0:2]]))


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
