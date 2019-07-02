def score(x, y):
    dist_squared = x*x + y*y
    if dist_squared > 100:
        return 0
    elif dist_squared > 25:
        return 1
    elif dist_squared > 1:
        return 5
    else:
        return 10
