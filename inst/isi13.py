import numpy as np
def isi13(A):
    b = np.asmatrix(A)
    r = np.dot(b,b)
    r = r.tolist()
    for line in r:
        print line
    return r
