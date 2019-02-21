import numpy as np
import datetime
import json
import socket
import datetime
import hashlib
import sys
from matplotlib import mlab


'''
Helper functions
'''

def get_psd(d, fs, nfft):
  power, freq = mlab.psd(d, Fs=fs, NFFT=nfft)
  num_bins = 128
  window_width = len(power)/num_bins
  power_ret = []
  freq_ret = []
  for i in range(num_bins):
    start = i*window_width
    stop = start + window_width 
    power_ret.append(power[start:stop].max())
    freq_ret.append(freq[start:stop].mean())
  return np.asarray(power_ret), np.asarray(freq_ret)

def sha256_checksum(filename, block_size=65536):
    sha256 = hashlib.sha256()
    with open(filename, 'rb') as f:
        for block in iter(lambda: f.read(block_size), b''):
            sha256.update(block)
    return sha256.hexdigest()

def ph_stats(vals, stable_threshold, N_samples):
  expval = np.exp(1j*np.asarray(vals)*np.pi/6.)
  m = np.angle(np.mean(expval))
  s = np.abs(expval.sum())/(1.*len(vals))
  if m<0:
    m += 2*np.pi
  mean_rounded = np.int(np.round(m/(2*np.pi)*12))
  return [mean_rounded, s, stable_threshold, N_samples, int(s>stable_threshold)]

def mean_stats(vals,mean_threshold):
  m = np.mean(vals)
  return [m, mean_threshold, int(abs(m-0.5)<mean_threshold)]

def mkdir_p(path): # Emulate mkdir -p functionality in python
  import os, errno
  try:
    os.makedirs(path)
  except OSError as exc:
    if exc.errno == errno.EEXIST and os.path.isdir(path):
      pass
    else: raise

def create_timestamp_and_path(base_path):
  ts = datetime.datetime.utcnow()   # Timestamp information for directory structure
  # Create a meaningful directory structure to organize recorded data
  p = base_path + '/' + str(ts.year) +'/' + str(ts.month) + '/' + str(ts.day) + '/'
  mkdir_p(p)
  # Call timestamp again (the directory name will not have changed, but the timestamp will be more accurate)
  ts = datetime.datetime.utcnow()
  return ts, p

def get_status_json(tart_instance):
  '''Generate JSON from status'''
  vals = tart_instance.read_status(False)
  d = tart_instance.extract(vals)
  d['timestamp (UTC)'] = datetime.datetime.utcnow().isoformat()
  d_json = json.dumps(d)
  return d, d_json


def run_diagnostic(tart, runtime_config):

    pp = tart.load_permute()
    print("Enabling DEBUG mode")
    tart.debug(on=not runtime_config['acquire'] , shift=runtime_config['shifter'], count=runtime_config['counter'], noisy=runtime_config['verbose'])
    print("Setting capture registers:")

    num_ant = runtime_config['diagnostic']['num_ant']
    N_samples = runtime_config['diagnostic']['N_samples']       # Number of samples for each antenna
    stable_threshold= runtime_config['diagnostic']['stable_threshold'] # 95% in same direction

    phases = []

    for src in range(num_ant):
      tart.reset()
      tart.capture(on=True, source=src, noisy=runtime_config['verbose'])
      tart.centre(runtime_config['centre'], noisy=runtime_config['verbose'])
      tart.start(runtime_config['diagnostic']['N_samples_exp'], True)
      k=0
      measured_phases = []
      while k<N_samples:
        k+=1
        d, d_json = get_status_json(tart)
        measured_phases.append(d["TC_STATUS"]['phase'])

      phases.append(dict(zip(['measured','stability','threshold','N_samples','ok'],ph_stats(measured_phases, stable_threshold, N_samples))))

    mean_phases = []
    for i in range(num_ant):
      mean_phases.append(phases[i]['measured'])

    print('median:', np.median(mean_phases))
    delay_to_be_set = (np.median(mean_phases) + 6) %12
    print('set delay to:', delay_to_be_set)

    runtime_config['sample_delay'] = delay_to_be_set

    print('small test acquisition')
    tart.reset()
    tart.debug(on=False, noisy=runtime_config['verbose'])
    tart.set_sample_delay(delay_to_be_set)
    tart.capture(on=True, source=0, noisy=runtime_config['verbose'])
    tart.centre(runtime_config['centre'], noisy=runtime_config['verbose'])
    tart.start_acquisition(1.1, True)

    d, d_json = get_status_json(tart)

    while not tart.data_ready():
      tart.pause(duration=0.005, noisy=True)
    print('\nAcquisition complete, beginning read-back.')
    #tart.capture(on=False, noisy=runtime_config['verbose'])
    print(runtime_config['diagnostic']['spectre']['N_samples_exp'])
    data = tart.read_data(num_words=2**runtime_config['diagnostic']['spectre']['N_samples_exp'])
    data = np.asarray(data,dtype=np.uint8)
    ant_data = np.flipud(np.unpackbits(data).reshape(-1,24).T)
    print(ant_data[:,:10])
    radio_means = []
    mean_threshold = 0.2
    for i in range(num_ant):
      radio_means.append(dict(zip(['mean','threshold','ok'],mean_stats(ant_data[i],mean_threshold))))

    ant_data = np.asarray(ant_data,dtype=np.float16)*2-1.

    channels = []

    for i in range(num_ant):
      channel = {}
      channel['id'] = i
      channel['phase'] = phases[i]
      channel['radio_mean'] =radio_means[i]
      power, freq = get_psd(ant_data[i]-ant_data[i].mean(),16e6,runtime_config['diagnostic']['spectre']['NFFT'])
      power_db = 10.*np.log10(power)
      power_db = np.nan_to_num(power_db)
      channel['power'] = (np.asarray(power_db*1000,dtype=np.int)/1000.    ).tolist()
      channel['freq'] = ((freq/1e6)).tolist()
      channels.append(channel)

    runtime_config['channels'] = channels
    runtime_config['channels_timestamp'] = datetime.datetime.utcnow()
    runtime_config['status'] = d


    print("\nDone.")
'''
RUN TART in raw data acquisition mode
'''

def run_acquire_raw(tart, runtime_config):
    runtime_config['acquire'] = 1
    tart.reset()
    tart.debug(on=not runtime_config['acquire'], \
                shift=runtime_config['shifter'], \
                count=runtime_config['counter'], \
                noisy=runtime_config['verbose'])
    tart.capture(on=True, source=0, noisy=runtime_config['verbose'])
    tart.set_sample_delay(runtime_config['sample_delay'])
    tart.centre(runtime_config['centre'], noisy=runtime_config['verbose'])
    t_stmp, path = create_timestamp_and_path(runtime_config['raw']['base_path'])
    tart.start_acquisition(1.1, True)

    while not tart.data_ready():
      tart.pause(duration=0.005, noisy=True)
    print('\nAcquisition complete, beginning read-back.')
    #tart.capture(on=False, noisy=runtime_config['verbose'])

    data = tart.read_data(num_words=np.power(2, runtime_config['raw']['N_samples_exp']))

    d, d_json = get_status_json(tart)
    runtime_config['status'] = d
    tart.reset()

    print('reshape antenna data')
    data = np.asarray(data,dtype=np.uint8)
    ant_data = np.flipud(np.unpackbits(data).reshape(-1,24).T)
    print(ant_data)

    if runtime_config['raw']['save']:
        from tart.operation import observation
        from tart.operation import settings
        config = settings.from_file(runtime_config['telescope_config_path'])
        filename = path + t_stmp.strftime('%H_%M_%S.%f') + '_data.pkl'
        print('create observation object')
        obs = observation.Observation(t_stmp, config, savedata=ant_data)
        obs.save(filename)
        print('saved to: ', filename)
        return {'filename':filename, 'sha256':sha256_checksum(filename)}
    return {}
    print('\nDone.')

