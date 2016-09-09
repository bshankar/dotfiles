# project euler-012


def sum_Of_factors(n):
    a = int(n**0.5)
    sumf = 1
    i = 2
    while n != 1:
        count = 1
        while n % i == 0:
            n = n / i
            count += 1
        sumf *= count
        i += 1
    return sumf


def first_triangular_number(n):
    i = 1
    while True:
        a = i * (i + 1) // 2
        if sum_Of_factors(a) > n:
            return a
        i += 1


print(first_triangular_number(500))
