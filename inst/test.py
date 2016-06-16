def test(num):
    result = 0
    for i in range(num):
        for j in range(num):
            result = result + i + j
    return result
