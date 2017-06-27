import numpy as np

'''
Helper functions
'''
def rr(n,prec=3):
  f=10.**prec
  return int(n*f)/f

def ph_stats(vals, stable_threshold, N_samples):
  expval = np.exp(1j*np.asarray(vals)*np.pi/6.)
  m = np.angle(np.mean(expval))
  s = np.abs(expval.sum())/(1.*len(vals))
  if m<0:
    m += 2*np.pi
  mean_rounded = np.int(np.round(m/(2*np.pi)*12))
  return [mean_rounded, rr(s,3), stable_threshold, N_samples, int(s>stable_threshold)]

def mean_stats(vals,mean_threshold):
  m = np.mean(vals)
  return [rr(m,4), mean_threshold, int(abs(m-0.5)<mean_threshold)]

def mkdir_p(path): # Emulate mkdir -p functionality in python
  import os, errno
  try:
    os.makedirs(path)
  except OSError as exc:
    if exc.errno == errno.EEXIST and os.path.isdir(path):
      pass
    else: raise

def create_timestamp_and_path(base_path):
  import datetime
  ts = datetime.datetime.utcnow()   # Timestamp information for directory structure
  # Create a meaningful directory structure to organize recorded data
  p = base_path + '/' + str(ts.year) +'/' + str(ts.month) + '/' + str(ts.day) + '/'
  mkdir_p(p)
  # Call timestamp again (the directory name will not have changed, but the timestamp will be more accurate)
  ts = datetime.datetime.utcnow()
  return ts, p

'''
RUN TART in diagnose mode
'''

def run_diagnostic(tart, runtime_config):

    pp = tart.load_permute()
    print "Enabling DEBUG mode"
    tart.debug(on=not runtime_config['acquire'] , shift=runtime_config['shifter'], count=runtime_config['counter'], noisy=runtime_config['verbose'])
    print "Setting capture registers:"


    num_ant = runtime_config['diagnostic']['num_ant']
    N_samples = runtime_config['diagnostic']['N_samples']       # Number of samples for each antenna
    stable_threshold= runtime_config['diagnostic']['stable_threshold'] # 95% in same direction

    phases = []

    for src in range(num_ant):
      tart.reset()
      tart.capture(on=True, source=src, noisy=runtime_config['verbose'])
      tart.centre(runtime_config['centre'], noisy=runtime_config['verbose'])
      tart.start(runtime_config['blocksize'], True)
      k=0
      measured_phases = []
      while k<N_samples:
        k+=1
        d, d_json = tart.get_status_json()
        measured_phases.append(d["TC_STATUS"]['phase'])

      phases.append(dict(zip(['measured','stability','threshold','N_samples','ok'],ph_stats(measured_phases, stable_threshold, N_samples))))

    mean_phases = []
    for i in range(num_ant):
      mean_phases.append(phases[i]['measured'])

    print 'median:', np.median(mean_phases)
    delay_to_be_set = (np.median(mean_phases) + 6) %12
    print 'set delay to:', delay_to_be_set

    runtime_config['sample_delay'] = delay_to_be_set

    print 'small test acquisition'
    tart.reset()
    tart.debug(on=False, noisy=runtime_config['verbose'])
    tart.set_sample_delay(delay_to_be_set)
    tart.capture(on=True, source=0, noisy=runtime_config['verbose'])
    tart.centre(runtime_config['centre'], noisy=runtime_config['verbose'])
    tart.start_acquisition(1.1, True)

    d, d_json = tart.get_status_json()

    while not tart.data_ready():
      tart.pause(duration=0.005, noisy=True)
    print '\nAcquisition complete, beginning read-back.'
    #tart.capture(on=False, noisy=runtime_config['verbose'])

    data = tart.read_data(num_words=2**12)
    data = np.asarray(data,dtype=np.uint8)
    ant_data = np.flipud(np.unpackbits(data).reshape(-1,24).T)

    radio_means = []
    mean_threshold = 0.2
    for i in range(num_ant):
      radio_means.append(dict(zip(['mean','threshold','ok'],mean_stats(ant_data[i],mean_threshold))))

    channels = []

    for i in range(num_ant):
      channel = {}
      channel['id'] = i
      channel['phase'] = phases[i]
      channel['radio_mean'] =radio_means[i]
      channels.append(channel)

    runtime_config['channels'] = channels
    runtime_config['status'] = d

    print "\nDone."

'''
RUN TART in raw data acquisition mode
'''

def run_acquire_raw(tart, runtime_config):

    if not runtime_config.has_key('sample_delay'):
        print 'no sample_delay determinded yet. running diagnostic'
        run_diagnostic(tart, runtime_config)

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
    print '\nAcquisition complete, beginning read-back.'
    #tart.capture(on=False, noisy=runtime_config['verbose'])

    data = tart.read_data(num_words=np.power(2, runtime_config['raw']['N_samples_exp']))

    d, d_json = tart.get_status_json()
    runtime_config['status'] = d
    tart.reset()

    print 'reshape antenna data'
    data = np.asarray(data,dtype=np.uint8)
    ant_data = np.flipud(np.unpackbits(data).reshape(-1,24).T)
    print ant_data

    if runtime_config['raw']['save']:
        from tart.operation import observation
        from tart.operation import settings
        config = settings.Settings(runtime_config['raw']['config'])
        filename = path + t_stmp.strftime('%H_%M_%S.%f') + '_data.pkl'
        print 'create observation object'
        obs = observation.Observation(t_stmp, config, savedata=ant_data)
        obs.save(filename)
        print 'saved to: ', filename

    print '\nDone.'



def run_acquire_vis(tart, runtime_config):

    if not runtime_config.has_key('sample_delay'):
        print 'no sample_delay determinded yet. running diagnostic'
        run_diagnostic(tart, runtime_config)

    #tart.reset()
    #tart.debug(on=not runtime_config['acquire'], \
                #shift=runtime_config['shifter'], \
                #count=runtime_config['counter'], \
                #noisy=runtime_config['verbose'])
    #tart.capture(on=True, source=0, noisy=runtime_config['verbose'])
    #tart.set_sample_delay(runtime_config['sample_delay'])
    #tart.centre(runtime_config['centre'], noisy=runtime_config['verbose'])
    #t_stmp, path = create_timestamp_and_path(runtime_config['raw']['base_path'])
    #tart.start_acquisition(1.1, True)

    #while not tart.data_ready():
      #tart.pause(duration=0.005, noisy=True)
    #print '\nAcquisition complete, beginning read-back.'
    ##tart.capture(on=False, noisy=runtime_config['verbose'])

    #data = tart.read_data(num_words=np.power(2, runtime_config['raw']['N_samples_exp']))

    #d, d_json = tart.get_status_json()
    #runtime_config['status'] = d
    #tart.reset()

    #print 'reshape antenna data'
    #data = np.asarray(data,dtype=np.uint8)
    #ant_data = np.flipud(np.unpackbits(data).reshape(-1,24).T)
    #print ant_data

    #if runtime_config['raw']['save']:
        #from tart.operation import observation
        #from tart.operation import settings
        #config = settings.Settings(runtime_config['raw']['config'])
        #filename = path + t_stmp.strftime('%H_%M_%S.%f') + '_data.pkl'
        #print 'create observation object'
        #obs = observation.Observation(t_stmp, config, savedata=ant_data)
        #obs.save(filename)
        #print 'saved to: ', filename

    #print '\nDone.'

