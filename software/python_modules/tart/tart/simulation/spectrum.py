# -*- coding: utf-8 -*-
import numpy as np
from pylab import plot, show, title, xlabel, ylabel, subplot, legend
from scipy import fft, arange


def plotSpectrum(y, Fs, c="red", label="powerspectrum"):
    """
    Plots a Single-Sided Amplitude Spectrum of y(t)
    """
    n = len(y)  # length of the signal
    k = arange(n)
    T = n / Fs
    frq = k / T  # two sides frequency range
    frq = frq[list(range(n / 2))]  # one side frequency range

    Y = fft(y) / n  # fft computing and normalization
    Y = Y[list(range(n / 2))]

    plot(frq, abs(Y), c=c, label=label)  # plotting the spectrum
    xlabel("Freq (Hz)")
    ylabel("|Y(freq)|")
    return (frq, np.array(abs(Y)))


if __name__ == "__main__":
    Fs = 150.0
    # sampling rate
    Ts = 1.0 / Fs
    # sampling interval
    t = np.arange(0, 1, Ts)  # time vector

    ff = 5
    # frequency of the signal
    y = np.sin(2 * np.pi * ff * t)

    subplot(2, 1, 1)
    plot(t, y)
    xlabel("Time")
    ylabel("Amplitude")
    subplot(2, 1, 2)
    plotSpectrum(y, Fs)
    show()
