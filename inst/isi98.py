def isi98_python(M, nTries):
    n = len(M[0])






def fun(M):
    n = len(M[0])
    result = [0,0]
    k = M[:]
    counter = 0
    for i in range(1,n):
        temp = k[i][0:i]
        part_counter = len([x for x in temp if x >0])
        counter = part_counter + counter


