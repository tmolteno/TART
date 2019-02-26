#!/usr/bin/python

# Max Scheel (2013-2018)
# max@max.ac.nz
# About:
#   Generate frequency spectre for the TART telescope from raw data files.
# Usage:
#   python raw_data_info.py --file ~/data/23_36_51.819267_data.pkl --show --spectrum

import numpy as np
import sys, os, errno
import argparse

from tart.operation import observation

class PlotPresets():
  def __init__(self):
    self.colors = [(0.86, 0.3712, 0.34),\
                  (0.8288, 0.86, 0.34),\
                  (0.34, 0.86, 0.3712),\
                  (0.34, 0.8288, 0.86),\
                  (0.3712, 0.34, 0.86),
                  (0.86, 0.34, 0.8288)]
    self.linestyles = ['-','--','-.',':']
    self.linewidth = 2.
    
pp = PlotPresets()


def get_psd(d, fs, nfft):
  power, freq = mlab.psd(d, Fs=fs, NFFT=nfft)
  return power, freq

def plot_spectrum(power, freq, ax=None, fontsize=10, linestyle='-', color='blue', linewidth=2):
  if ax is None:
    f, ax = plt.subplots()

  ax.plot(freq/1e6, 10.0*np.log10(power), linestyle=linestyle, color=color, linewidth=linewidth)
  ax.set_xticks(np.arange(1,8))
  ax.set_yticks([-75,-70,-65])
  ax.set_xlim([0, 8])
  ax.set_ylim([-80, -60])
  ax.grid()
  ax.set_xlabel("Frequency [MHz]", fontsize=fontsize)
  ax.set_ylabel("PSD [dB/Hz]", fontsize=fontsize)

def calc_ant_spec(i,fs,nfft):
  power, freq = get_psd(d_corrected[i], fs, nfft=nfft)
  print('Done', i)
  return [power, freq]


if __name__ == '__main__':
  PARSER = argparse.ArgumentParser(description='Generate frequency spectre for the TART telescope from raw data files.')
  PARSER.add_argument('--file', required=True, help="The file for the telescope data.")
  PARSER.add_argument('--plot', action="store_true", help="Plot (a few) the data samples.")
  PARSER.add_argument('--means', action="store_true", help="Show means of antenna data.")
  PARSER.add_argument('--spectrum', action="store_true", help="Plot the power spectral density.")
  PARSER.add_argument('--exp', required=False, default=14, type=int, help="NFFT exponent.")
  PARSER.add_argument('--show', action="store_true",  help="Show plots.")
  PARSER.add_argument('--single', action="store_true",  help="single spectrum.")
  PARSER.add_argument('--multiprocessing', action="store_true",  help="use multiple processes.")
  PARSER.add_argument('--fout', required=False, help="Output file name.")

  ARGS = PARSER.parse_args()
  fname = ARGS.file
  if not ARGS.show:
    import matplotlib
    matplotlib.use('agg')


  import matplotlib.pyplot as plt
  from matplotlib import mlab
  import scipy

  nobs = observation.Observation_Load(fname)
  NUM_ANTENNAS = nobs.config.get_num_antenna()
  print("Date %s " % str(nobs.timestamp))
  print("Number of Points %d " % len(nobs.data[0]))
  print("Number of Antenna %d" % NUM_ANTENNAS)
  fs =  nobs.get_sampling_rate()
  print("Sampling rate %d" % fs)
  print(nobs.get_antenna(0))


  d_corrected = []
  for i in range(NUM_ANTENNAS):
    d = nobs.get_antenna(i)
    d_corrected.append(d - d.mean())
  print('done mean correction')

  #from plotpresets import PlotPresets
  #pp = PlotPresets()

  if ARGS.means:
    for i, x in enumerate(nobs.config.ant_positions):
      print(nobs.get_means())
      print("   %d ant loc %s, mean %g" % (i, x, nobs.get_antenna(i).mean()))

  title = "TART data (%s)" % str(nobs.timestamp)
  if (ARGS.plot):
    plt.close('all')
    n = len(nobs.get_antenna(0))
    t = np.linspace(0, n/fs, n) * 1.0e6
    f, axarr = plt.subplots(NUM_ANTENNAS, sharex=True)
    l = 100
    for i in range(NUM_ANTENNAS):
      axarr[i].plot(t[0:l], nobs.get_antenna(i)[0:l])
      axarr[i].set_ylabel("Channel %d" % i)
      axarr[i].set_xlabel("t (us)")
    plt.suptitle(title)
    fname = 'data_plot.pdf'
    plt.savefig(fname)

  if ARGS.single:
    power, freq = get_psd(nobs.get_antenna(0), fs, nfft=2**ARGS.exp)
    plot_spectrum(power,freq, fontsize=9)
    #plt.show()

  if (ARGS.spectrum):
    plt.close('all')
    try:
      import seaborn as sns
      sns.set(style="whitegrid")
      sns.set_palette('hls', n_colors=24)
    except:
      print('no seaborn found')

    f, axarr = plt.subplots(4, 6, sharey=True, sharex=True)
    f.suptitle(title)
    f2, axarr2 = plt.subplots(1)

    if ARGS.multiprocessing:
      from multiprocessing import Pool
      p = Pool(8)
      resultList = []
      ret = []
      try:
        for i in range(NUM_ANTENNAS):
          print('deployed', i)
          resultList.append(p.apply_async(calc_ant_spec, (i,fs,2**ARGS.exp)))
        p.close()
        p.join()
        for sv_thread in resultList:
          res = sv_thread.get()
          if (res != None):
            ret.append(res)
            print('.',)
            sys.stdout.flush()
        p.terminate()
      except KeyboardInterrupt:
        print('control-c pressed')
        p.terminate()
      print(ret)
      for i in range(NUM_ANTENNAS):
        power, freq = ret[i]
        ax = axarr.ravel()[i]
        plot_spectrum(power, freq, ax=ax, fontsize=9)
        plot_spectrum(power, freq, ax=axarr2, color=pp.colors[i%6], linestyle=pp.linestyles[i/6], linewidth=pp.linewidth, fontsize=9)
        ax.legend([i,],fontsize=9, loc=3)

    else:
      for i in range(NUM_ANTENNAS):
        print(i)
        ax = axarr.ravel()[i]
        d = nobs.get_antenna(i)
        d_corrected = d - d.mean()
        power, freq = get_psd(d_corrected, fs, nfft=2**ARGS.exp)
        plot_spectrum(power, freq, ax=ax, fontsize=9)
        plot_spectrum(power, freq, ax=axarr2, color=pp.colors[i%6], linestyle=pp.linestyles[i/6], linewidth=pp.linewidth, fontsize=9)
        ax.legend([i,],fontsize=9, loc=3)

    axarr2.legend(np.arange(24), ncol=2)
    f.tight_layout()
    f.subplots_adjust(wspace=0.0, hspace=0.0)
    PLOT_FNAME = "snapshot_spectrum_grid.pdf"
    f.savefig(PLOT_FNAME)
    print('saved to: ', PLOT_FNAME)
    PLOT_FNAME = "snapshot_spectrum_multiple.pdf"
    f2.tight_layout()
    f2.savefig(PLOT_FNAME)
    print('saved to: ', PLOT_FNAME)

  if ARGS.show:
    plt.show()
