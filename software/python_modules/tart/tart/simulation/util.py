import math


class Util(object):
    PI = 3.1415926535897932
    PI2 = 6.2831853071795864
    C = 2.99793e8

    @classmethod
    def check_t(self, t):
        half_week = 302400.0
        tt = t

        if t > half_week:
            tt = t - 2 * half_week
        if t < -half_week:
            tt = t + 2 * half_week

        return tt

    @classmethod
    def idiv(self, x, y):
        """ Integer Division """
        return int(math.floor(x / y))

    @classmethod
    def rem(self, x, y):
        """ Remainder """
        return x - y * Util.idiv(x, y)

    #! \brief remainder = numerator - quotient * denominator
    #
    @classmethod
    def mod(self, x, y):
        ret = x - y * Util.idiv(x, y)
        return ret

    @classmethod
    def mod_int(self, x, y):
        ret = x % y
        if x < 0:
            ret = y - (-x) % y
        return ret

    @classmethod
    def rad2deg(self, x):
        return x * 180.0 / Util.PI

    @classmethod
    def rem2pi(self, x):
        return Util.rem(x, Util.PI2)
