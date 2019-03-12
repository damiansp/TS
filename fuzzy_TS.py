def triangular(x, a, b, c):
    '''
    Defines a triangle with points: left: (a, 0), top: (b, 1), right: (c, 0);
    x is a value to be assigned affinity to the group defined by abc
    '''
    return max(min((x - a)/(b - a), (c - x)/(c - b)), 0)

