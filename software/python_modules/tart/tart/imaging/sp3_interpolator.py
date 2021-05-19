# Interpolator for sp3 orbit files
# Copyright 2013 (c) Tim Molteno. tim@elec.ac.nz


class Sp3Interpolator:
    def __init__(self, gpst, points):
        self.interps = points
        self.week = gpst.week()

    def lagrange_numerator(self, t_i, i, t):
        n = len(t_i)
        ret = 1.0
        for j in range(0, n):
            if j != i:
                ret *= t - t_i[j]
        return ret

    def lagrange_denominator(self, t_i, i):
        n = len(t_i)
        ret = 1.0
        for j in range(0, n):
            if j != i:
                ret *= t_i[i] - t_i[j]
        return ret

    def lagrange_interpolate(self, t_i, y_i, t):
        ret = 0.0
        n = len(t_i)
        for i in range(0, n):
            ret += y_i[i] * (
                self.lagrange_numerator(t_i, i, t) / self.lagrange_denominator(t_i, i)
            )
        return ret

    def get_sv_position(self, gt, sv):
        sow = gt.sow()

        inter = self.interps[sv]

        dt0 = 999999.0
        i_close = 0
        n = len(inter)
        for j in range(0, n):
            i = inter[j]
            test = abs(i[0] - sow)
            if (i[0] <= sow) and (test < dt0):
                dt0 = test
                i_close = j

        t_points = []
        x_points = []
        y_points = []
        z_points = []

        order = 5
        i0 = max(0, i_close - order)
        i1 = min(i_close + order + 1, n - 1)

        for i in range(i0, i1):
            t_points.append(inter[i][0])
            x_points.append(inter[i][1])
            y_points.append(inter[i][2])
            z_points.append(inter[i][3])

        xl = self.lagrange_interpolate(t_points, x_points, sow) * 1000.0
        yl = self.lagrange_interpolate(t_points, y_points, sow) * 1000.0
        zl = self.lagrange_interpolate(t_points, z_points, sow) * 1000.0

        return [xl, yl, zl]
