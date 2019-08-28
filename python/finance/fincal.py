def pvFunction(fv, r, n):
    return fv / (1 + r)**n

def pvPerpetuity(c, r):
    return c / r

def pvPerpetuityDue(c, r):
    return c / r*(1 + r)

def pvAnnuity(c, r, n):
    return c / r*(1 - 1/(1 + r)**n)

def pvAnnuityDue(c, r, n):
    return c / r*(1 - 1/(1 + r)**n)*(1 + r)

def pvGrowingAnnuity(c, r, n, g):
    return c / (r - g)*(1 - (1 + g)**n/(1 + r)**n)

def fvFunction(pv, r, n):
    return pv * (1 + r)**n

def fvAnnuity(cv, r, n):
    return c / r*((1 + r)**n - 1)

def fvAnnuityDue(cv, r, n):
    return c / r * ((1 + r)**n - 1) * (1 + r)

def fvGrowingAnnuity(cv, r, n):
    return c / (r - g) * ((1 + r)**n - (1 + g) * n)
