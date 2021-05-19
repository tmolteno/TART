from sympy import *

r = Symbol("r")
el = Symbol("el")
az = Symbol("az")
a_n = Symbol("a_n")
a_e = Symbol("a_e")
a_z = Symbol("a_z")
c = Symbol("c")


def norm(arr):
    return sqrt(arr[0] * arr[0] + arr[1] * arr[1] + arr[2] * arr[2])


S = [r, el, az]
dt_0 = r / c
ant_nez = [a_n, a_e, a_z]
S_nez = [r * cos(az) * cos(el), r * sin(az) * cos(el), r * sin(el)]
delta_nez = [S_nez[0] - ant_nez[0], S_nez[1] - ant_nez[1], S_nez[2] - ant_nez[2]]
dt_ant = norm(delta_nez) / c

dt = dt_ant - dt_0

print(simplify(dt))

from math import sin, cos


def delay(r, el, az, a_n, a_e, a_z):
    c = 2.99793e8
    return (
        -r
        + (
            a_e ** 2
            - 2.0 * a_e * r * sin(az) * cos(el)
            + a_n ** 2
            - 2.0 * a_n * r * cos(az) * cos(el)
            + a_z ** 2
            - 2.0 * a_z * r * sin(el)
            + r ** 2
        )
        ** (1.0 / 2.0)
    ) / c


def assert_equal(x, y):
    dx = abs(x - y)
    if dx > 1e-5 * (abs(x) + abs(y)):
        print("FAIL %g != %g (%f)" % (x, y, dx))
    else:
        print("PASS %g == %g (%f)" % (x, y, dx))


r = 1e10


def to_rad(xx):
    return xx * 3.1415926 / 180.0


c = 2.99793e8
r = 1e9

assert_equal(delay(r, to_rad(90.0), to_rad(45), 0.0, 1.0, 0.0), 0.0)
assert_equal(delay(r, to_rad(45.0), to_rad(0), 0.0, 1.0, 0.0), 0.0)
assert_equal(delay(r, to_rad(0.0), to_rad(90), 0.0, 1.0, 0.0), -1.0 / c)
assert_equal(delay(r, to_rad(45.0), to_rad(45), 0.0, 1.0, 0.0), -1.6678e-9)
