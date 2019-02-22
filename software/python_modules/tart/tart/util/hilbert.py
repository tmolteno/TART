
import time

import numpy as np
import pyfftw


from pyfftw.interfaces.scipy_fftpack import hilbert as scipy_fftpack_hilbert

def hilbert(s, debug=False):
    '''
    http://au.mathworks.com/help/signal/ref/hilbert.html
    The analytic signal for a sequence x has a one-sided Fourier transform. That is, the transform vanishes for negative frequencies. To approximate the analytic signal, hilbert calculates the FFT of the input sequence, replaces those FFT coefficients that correspond to negative frequencies with zeros, and calculates the inverse FFT of the result.

    In detail, hilbert uses a four-step algorithm:

    It calculates the FFT of the input sequence, storing the result in a vector x.
    It creates a vector h whose elements h(i) have the values:
    1 for i = 1, (n/2)+1
    2 for i = 2, 3, ... , (n/2)
    0 for i = (n/2)+2, ... , n
    It calculates the element-wise product of x and h.
    It calculates the inverse FFT of the sequence obtained in step 3 and returns the first n elements of the result.
    This algorithm was first introduced in [2]. The technique assumes that the input signal, x, is a finite block of data. This assumption allows the function to remove the spectral redundancy in x exactly. Methods based on FIR filtering can only approximate the analytic signal, but they have the advantage that they operate continuously on the data. See Single-Sideband Amplitude Modulation for another example of a Hilbert transform computed with an FIR filter.'''

    s_0 = time.time()
    S = np.fft.fft(s)
    if debug:
        print('fft', time.time()-s_0)
    s_1 = time.time()

    n = len(s)
    h = np.zeros(n)
    h[0] = 1.
    h[n/2] = 1.
    h[1:n/2] = 2.
    #h[n/2+1:] = 0.
    if debug:
        print('setup', time.time()-s_1)
    s_2 = time.time()
    ret = np.fft.ifft(h*S)[:n].imag
    if debug:
        print('ifft', time.time()-s_2)

    return ret


def hilbert_fftw(s, debug=False, dtype='complex64'):
    ''' fftw drop replacement for scipy_fftpack.hilbert
    beware of sign of returned seq. is '-'

    http://au.mathworks.com/help/signal/ref/hilbert.html
    The analytic signal for a sequence x has a one-sided Fourier transform. That is, 
    the transform vanishes for negative frequencies. To approximate the analytic signal, 
    hilbert calculates the FFT of the input sequence, replaces those FFT coefficients 
    that correspond to negative frequencies with zeros, and calculates the inverse FFT of the result.

    In detail, hilbert uses a four-step algorithm:

    It calculates the FFT of the input sequence, storing the result in a vector x.
    It creates a vector h whose elements h(i) have the values:
    1 for i = 1, (n/2)+1
    2 for i = 2, 3, ... , (n/2)
    0 for i = (n/2)+2, ... , n
    It calculates the element-wise product of x and h.
    It calculates the inverse FFT of the sequence obtained in step 3 and returns the first n elements of the result.
    This algorithm was first introduced in [2]. The technique assumes that the input signal, x, 
    is a finite block of data. This assumption allows the function to remove the spectral redundancy in x exactly. 
    Methods based on FIR filtering can only approximate the analytic signal, but they have the advantage that 
    they operate continuously on the data. See Single-Sideband Amplitude Modulation for another example of a 
    Hilbert transform computed with an FIR filter.
    '''

    n = len(s)
    pyfftw.interfaces.cache.enable()
    pyfftw.interfaces.cache.set_keepalive_time(50.0)
    align = pyfftw.simd_alignment

    write_wisdom = False
    try:
        wisdom = pickle.load(open("wisdom_hilbert.wis", "rb"))
        pyfftw.import_wisdom(wisdom)
    except:
        write_wisdom = True
        print('no wisdom file')

    fft_in = pyfftw.empty_aligned(n, dtype=dtype, n=align)
    fft_out = pyfftw.empty_aligned(n, dtype=dtype, n=align)
    ifft_in = pyfftw.empty_aligned(n, dtype=dtype, n=align)
    ifft_out = pyfftw.empty_aligned(n, dtype=dtype, n=align)
    fft_machine = pyfftw.FFTW(fft_in, fft_out, direction='FFTW_FORWARD', flags=('FFTW_ESTIMATE',), threads=8)
    ifft_machine = pyfftw.FFTW(ifft_in, ifft_out, direction='FFTW_BACKWARD', flags=('FFTW_ESTIMATE',), threads=8)

    if write_wisdom:
        wisdom = pyfftw.export_wisdom()
        pickle.dump(wisdom, open("wisdom_hilbert.wis", "wb"))

    s_0 = time.time()
    fft_in[:] = s
    S = fft_machine()
    if debug:
        print('fft', time.time()-s_0)

    s_1 = time.time()
    h = np.zeros(n)
    h[0] = 1.
    h[n/2] = 1.
    h[1:n/2] = 2.
    if debug:
        print('setup', time.time()-s_1)
    s_2 = time.time()
    ifft_in[:] = h*S
    ret = ifft_machine()
    if debug:
        print('ifft', time.time()-s_2)
    return -ret[:n].imag


if __name__ == '__main__':
    x = np.random.randint(0, 2, 2**21)*2-1
    start = time.time()
    a = -hilbert(x)
    print('hilbert took', time.time()-start)
    start = time.time()
    b = hilbert_fftw(x, debug=False)
    print('max took', time.time()-start)

    start = time.time()
    c = scipy_fftpack_hilbert(x)
    print('hilbert fftw took', time.time()-start)

    a = np.sign(a)
    b = np.sign(b)
    c = np.sign(c)
    print(np.allclose(a, b))
    print(np.allclose(b, c))
    print(np.allclose(a, c))
