from tart.operation import observation
from tart.imaging import visibility
from tart.util import angle

import numpy as np

# import pyfftw
# from pyfftw.interfaces.scipy_fftpack import hilbert as fftw_hilbert
# from scipy.fftpack import hilbert as fftw_hilbert
# from tart.util.hilbert import hilbert_fftw as fftw_hilbert
import time


def van_vleck_correction(R):
    return np.sin(np.pi / 2.0 * R)


def combine_real_imag(v_real, v_imag):
    return v_real - 1j * v_imag


class Correlator(object):
    def __init__(self, van_vleck_corr=True):
        self.vv = van_vleck_corr

    def correlate(self, obs, debug=False, mode="roll"):
        visibilities, baselines = self.compute_complex_vis(obs, debug=debug, mode=mode)
        vis = visibility.Visibility(obs.config, obs.timestamp)
        vis.set_visibilities(visibilities, baselines)
        return vis

    def compute_complex_vis(self, obs, debug=False, mode="roll"):
        """Return an array of baselines and visibilities from this observation"""
        v = []
        baselines = []
        data = []
        data_hilb = []
        num_antenna = obs.config.get_num_antenna()
        for i in range(num_antenna):
            ant_i = obs.get_antenna(i)
            mean_i = np.mean(ant_i)
            data.append(ant_i - mean_i)

        if "fftw" in mode:
            import pyfftw
            from pyfftw.interfaces.scipy_fftpack import hilbert as fftw_hilbert

        for i, d in enumerate(data):
            if mode == "roll":
                data_hilb.append(np.roll(d, 1))
            elif mode == "fftw_hilbert":
                h_i = -fftw_hilbert(d)
                data_hilb.append(h_i)
            elif mode == "fftw_hilbert_sign":
                h_i = -np.sign(fftw_hilbert(d))
                data_hilb.append(h_i)

        for i in range(0, num_antenna):
            for j in range(i + 1, num_antenna):
                v.append(self.V(data[i], data[j], data_hilb[j]))
                baselines.append([i, j])
        return v, baselines

    def V(self, x, y, yhilb):
        v_real = np.dot(x, y) * 1.0 / float(len(x))
        v_imag = np.dot(x, yhilb) * 1.0 / float(len(x))
        if self.vv:
            v_real = van_vleck_correction(v_real)
            v_imag = van_vleck_correction(v_imag)
        return combine_real_imag(v_real, v_imag)

    # def correlate_roll(self, obs, debug=False):
    # v = []
    # baselines = []
    # num_ant = obs.config.get_num_antenna()
    # data = obs.data # use obs.data[i] instead of get_antenna to keep mem usage low
    # means = obs.get_means()
    # cos_str = []
    # sin_str = []
    # for a in data:
    # cos_str.append(a[1:].tostring())
    # sin_str.append(a[:-1].tostring())
    # n = len(data[0][1:])
    # for i in range(0, num_ant):
    # for j in range(i+1, num_ant):
    # if debug:
    # progress = len(baselines)
    # if (progress%10==0):
    # print(progress)
    ##cos_i = data[i][1:]
    ##cos_j = data[j][1:]
    ##sin_j = data[j][:-1]
    ##v_real =    -means[i]*means[j] + corr_b(cos_i, cos_j, n)
    ##v_imag =    -means[i]*means[j] + corr_b(cos_i, sin_j, n)
    # v_real =    -means[i]*means[j] + corr_b_cpp(cos_str[i], cos_str[j], n)
    # v_imag =    -means[i]*means[j] + corr_b_cpp(cos_str[i], sin_str[j], n)

    # if self.vv:
    # v_real = van_vleck_correction(v_real)
    # v_imag = van_vleck_correction(v_imag)
    # v_com = combine_real_imag(v_real,v_imag)
    # v.append(v_com)
    # baselines.append([i,j])
    # vis = visibility.Visibility(obs.config, obs.timestamp)
    # vis.set_visibilities(v, baselines)
    # return vis


def corr_b(x, y, n):
    # num_not_same = (x ^ y).sum()
    num_not_same = np.count_nonzero(x ^ y)  # alot faster than sum.
    ret = 1 - 2 * num_not_same / float(n)
    return ret


def corr_b_pat(x, y):
    n = len(x)
    num_same = (x ^ (1 - y)).sum()
    ret = 2 * num_same / float(n) - 1
    return ret


# from scipy import weave

# code = """
# PyObject* res = PyString_FromStringAndSize(NULL, real_size);

# const ssize_t tail = (ssize_t)PyString_AS_STRING(res) % ALIGNMENT;
# const ssize_t head = (ALIGNMENT - tail) % ALIGNMENT;

# memxor((const char*)a, (const char*)b, PyString_AS_STRING(res), head);

# const __m128i* pa = (__m128i*)((char*)a + head);
# const __m128i* pend = (__m128i*)((char*)a + real_size - tail);
# const __m128i* pb = (__m128i*)((char*)b + head);
# __m128i xmm1, xmm2;
# __m128i* pc = (__m128i*)(PyString_AS_STRING(res) + head);
# while (pa < pend) {
# xmm1 = _mm_loadu_si128(pa);
# xmm2 = _mm_loadu_si128(pb);
# _mm_stream_si128(pc, _mm_xor_si128(xmm1, xmm2));
# ++pa;
# ++pb;
# ++pc;
# }
# memxor((const char*)pa, (const char*)pb, (char*)pc, tail);
# return_val = res;
# Py_DECREF(res);
# """

# support = """
##define ALIGNMENT 16
# static void memxor(const char* in1, const char* in2, char* out, ssize_t n) {
# const char* end = in1 + n;
# while (in1 < end) {
# *out = *in1 ^ *in2;
# ++in1;
# ++in2;
# ++out;
# }
# }
# """

# def corr_b_cpp(x, y, n):
# a = np.frombuffer(x, dtype=np.bool)
# b = np.frombuffer(y, dtype=np.bool)
# real_size = len(a)
# out = weave.inline(code, ["a", "b", "real_size"],
# headers = ['"emmintrin.h"'],
# support_code = support)
# num_not_same = np.count_nonzero(np.frombuffer(out,dtype=np.bool))
# ret = 1 - 2*num_not_same/float(n)
# return ret


from tart.util import constants


class FxCorrelator(Correlator):
    """A FX correlator.
    We need an FX correlator because the bandwidth is sufficient that it washes out the fringes away
    from the phase center. The distance we can get away from the phase center is given by

    q = arcsin(c / B D)

    where B is the baseline length, D is the bandwidth, c is the speed of light.

    where

           c = 3e8 (m s^-1)
           B = baseline (m)
           D = bw (s^-1)
    """

    def __init__(self, bandwidth, linewidth):
        self.bandwidth = bandwidth
        n_log2 = int(np.log(bandwidth / linewidth) / np.log(2.0) + 0.5)
        self.n_freq = 2 ** n_log2
        self.linewidth = bandwidth / self.n_freq

    def get_linewidth(self):
        return self.linewidth

    def correlate(obs):
        data = self.convert_to_baseband(obs)
        vis = visibility.Visibility(obs, angle.from_dms(90), angle.from_dms(0))
        visibilities, baselines = self.compute_complex_vis(obs)
        vis.set_visibilities(visibilities, baselines)
        return vis

    """TODO this is not working yet.
         this should return the maximum field of view possible before fringes get washed
         out by the bandwidth of the signal."""

    def angular_resolution(self, baseline):
        x = constants.V_LIGHT / (self.linewidth * baseline)
        if x >= 1.0:
            x = 1.0
        return angle.asin(x)
