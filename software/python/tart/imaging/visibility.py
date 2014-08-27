from tart.operation import observation
import numpy as np

import pickle
from tart.util import angle
from tart.simulation import antennas
from tart.util import skyloc
from tart.util import constants
 
class Visibility:
  """
  A container class for visibilities from a single observation.
  """
  def __init__(self, obs, phase_el, phase_az):
    self.phase_el = phase_el
    self.phase_az = phase_az
    self.config = obs.config
    self.timestamp = obs.timestamp
    
  def set_visibilities(self, v, b):
    self.baselines = b
    self.v = v
    
  '''Rotated one, aimed at ra, decl
  
  Justification:
  
  Let
    s_1(t) = N(t) e^{j \omega t}
    
  t_g^{01) = t_g^1 - t_g^0 (arrival at a1 - arrival at a0)
  if t_g^{01} < 0, then s_0 arrives later than s_1 (defined by antennas.get_geo_delay_horizontal) 
  therefore:
  
    s_0(t) = s_1(t - t_g^{01})
  
  v(0,1) = <s_0(t) s_1^{*}(t)>
         = <s_1(t - t_g^{01}) s_1^{*}(t)>
         = <N(t) e^{j \omega t} e^{-j \omega t_g^{01}} N^*(t) e^{-j \omega t}>
         = e^{-j \omega t_g^{01}} <N(t) N^*(t)>
         
  So, after rotation
  
   <N(t) N^*(t)> = v(0,1) e^{+j \omega t_g^{01}} 
  '''
  def rotate(self, sloc):
    stopped_vis = []
    omega = self.config.frequency*2.0*np.pi
    # Now we must do fringe stopping
    el, az = self.config.get_loc().equatorial_to_horizontal(self.timestamp, sloc.ra, sloc.dec)
    
    for v, b in zip(self.v, self.baselines):
      a0 = antennas.Antenna(self.config.get_loc(), self.config.ant_positions[b[0]])
      a1 = antennas.Antenna(self.config.get_loc(), self.config.ant_positions[b[1]])

      tg = antennas.get_geo_delay_horizontal(a0, a1, el, az) 
      # tg is t_a1 - t_a0 
      # (negative if a1 is closer to source than a0)

      # print b, omega*tg
      v = v * np.exp(-1.0j * omega * tg)
      stopped_vis.append(v)
      
    self.phase_el = el
    self.phase_az = az
    self.v = stopped_vis

  def vis(self, i, j):
    if (j == i):
      raise "Baseline [%d,%d] is invalid" % (i,j)
    if (j < i): # The first index should be before the second
      return np.conjugate(self.vis(j,i))
    for k, b in enumerate(self.baselines):
      if (b == [i,j]):
        return self.v[k]
    raise "Baseline [%d,%d] is invalid" % (i,j)
    
  def get_closure_phase(self, i, j, k):
    return np.angle(self.vis(i,j)) + np.angle(self.vis(j,k)) - np.angle(self.vis(i,k))
  
  
  def toString(self):
    ret = ""
    for i,b in enumerate(self.baselines):
      ret += " V(%s)=%g, I%g" % (str(b), np.abs(self.v[i]), np.angle(self.v[i]))
    return ret
    

def Visibility_Save(vis, filename):
    save_data = open(filename, 'wb')
    pickle.dump(vis, save_data, pickle.HIGHEST_PROTOCOL)
    save_data.close()
    
def Visibility_Load(filename):
    load_data = open(filename, 'rb')
    ret = pickle.load(load_data)
    load_data.close()
    return ret

def Visibility_Lsq(vis1, vis2):
  """ Return least square based on the phases of 2 visibilities """
  if vis1.config.num_antennas == vis2.config.num_antennas:
    difflist = []
    for v1, v2 in zip(vis1.v, vis2.v):
      diff = np.abs(np.angle(v1) - np.angle(v2))
      if diff > np.pi:
        diff = 2.*np.pi-diff
      difflist.append(diff)
    diffarr = np.array(difflist)
    return np.power(diffarr,2).sum()

