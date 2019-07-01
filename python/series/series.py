def slices(series, length):
    if (length > len(series)):
        raise ValueError('length is longer then the length of series')
    elif (length < 1):
        raise ValueError('length must be greater than 0')

    output = []
    for i in range(len(series) - length + 1):
        output.append(series[i:(i+length)])

    return output
