def gauss(n):
    return (n * (n+1)) // 2

def gauss_m(n, m):
    return gauss((n-1)//m)*m

solution = gauss_m(1000, 3) + gauss_m(1000, 5) - gauss_m(1000, 15)
