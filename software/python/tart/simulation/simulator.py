"""Copyright (C) Max Scheel 2016. All rights reserved"""
import datetime
import argparse
import numpy as np
from multiprocessing import Pool

from tart.operation import settings

from tart.imaging import antenna_model
from tart.imaging import location
from tart.imaging import visibility
from tart.imaging import radio_source

from tart.simulation import skymodel
from tart.simulation import antennas
from tart.simulation import radio
from tart.imaging import correlator

from tart.util import angle
from copy import deepcopy

def get_vis_parallel(sky, cor, rad, ants, ant_models, config, t_list, mode='simp'):
  from multiprocessing import Pool
  p = Pool()
  resultList = []
  ret_vis_list = []
  try:
    for t in t_list:
      resultList.append(p.apply_async(get_vis, (sky, cor, rad, ants, ant_models, config, t, mode)))
    p.close
    p.join
    for thread in resultList:
      x2 = thread.get()
      if (x2 != None):
        ret_vis_list.append(x2)
    p.terminate()
  except KeyboardInterrupt:
    print 'control-c pressed'
    p.terminate()
  return ret_vis_list

def get_vis(sky, cor, rad, ants, ant_models, config, timestamp, mode='simp',seed=None):
  from tart.simulation import antennas
  np.random.seed(seed=seed)
  sources = sky.gen_photons_per_src( timestamp, radio=rad, config=config, n_samp=1)
  # sources = sky.gen_n_photons(config, timestamp, radio=rad, n=10)
  # print 'debug: total flux',  np.array([src.jansky(timestamp) for src in self.known_objects]).sum()
  # print 'debug: total amplitude', np.array([src.amplitude for src in sources]).sum()

  if mode == 'full':
    ant_sigs_full = antennas.antennas_signal(ants, ant_models, sources, rad.timebase)
    obs = rad.get_full_obs(ant_sigs_full, timestamp, config = config)
    v = cor.correlate(obs)
  elif mode == 'mini':
    v = antennas.antennas_simp_vis(ants, ant_models, sources, timestamp, config, rad.noise_level)
  else:
    ant_sigs_simp = antennas.antennas_simplified_signal(ants, ant_models, sources, rad.baseband_timebase, rad.int_freq, seed=seed)
    obs = rad.get_simplified_obs(ant_sigs_simp, timestamp, config = config, seed=seed)
    v = cor.correlate(obs)
  return v