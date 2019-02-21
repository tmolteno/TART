import spidev

import numpy as np
import time

import pkg_resources

PERMUTE_FILE=pkg_resources.resource_filename('tart_hardware_interface','permute.txt')

def tobin(arr):
  return [bin(i) for i in arr]


class TartSPI:
  '''Command for configuring, and querying TART hardware.'''

  ##--------------------------------------------------------------------------
  ##  TART SPI control, data, and status registers.
  ##--------------------------------------------------------------------------
  TC_CENTRE = 0x00
  TC_STATUS = 0x01
  TC_DEBUG  = 0x02
  TC_SYSTEM = 0x03

  AQ_STREAM = 0x20
  AQ_SYSTEM = 0x23

  VX_STREAM = 0x40
  VX_STATUS = 0x41
  VX_DEBUG  = 0x42
  VX_SYSTEM = 0x43

  SYS_STATS = 0x60
  SPI_STATS = 0x61
  SPI_RESET = 0x63

  NEWLINE   = 0x7F
  WRITE_CMD = 0x80

  LATENCY   = 2

  regs = [TC_CENTRE, TC_STATUS, TC_DEBUG, TC_SYSTEM,
          AQ_STREAM,                      AQ_SYSTEM,
          VX_STREAM, VX_STATUS, VX_DEBUG, VX_SYSTEM,
          SYS_STATS, SPI_STATS] #, self.SPI_RESET]

  regs_s= ['TC_CENTRE', 'TC_STATUS', 'TC_DEBUG', 'TC_SYSTEM',
          'AQ_STREAM',                          'AQ_SYSTEM',
          'VX_STREAM', 'VX_STATUS', 'VX_DEBUG', 'VX_SYSTEM',
          'SYS_STATS', 'SPI_STATS'] #, self.SPI_RESET]

  ##--------------------------------------------------------------------------
  ##  TART SPI interface commands.
  ##--------------------------------------------------------------------------
  def __init__(self, speed=32000000):
    self.spi = spidev.SpiDev()
    self.spi.open(0, 0)
    self.spi.mode = 0b00
    self.spi.bits_per_word = 8
    self.spi.max_speed_hz = int(speed)
    self.perm = None
    self.load_permute()

  def close(self, noisy=False):
    self.spi.close()
    if noisy:
      print('SPI <-> TART interface closed.')
    return 1

  def pause(self, duration=0.005, noisy=False):
    if noisy:
      print(' pausing for %1.3fs' % duration)
    time.sleep(duration)


  ##------------------------------------------------------------------------##
  ##  TART register read/write commands.
  ##------------------------------------------------------------------------##
  def setbyte(self, reg, val, noisy=False):
    reg = int(reg) & 0x7f
    self.spi.xfer([self.WRITE_CMD | reg, val])
    if noisy:
      self.getbyte(reg, noisy)
    return True

  def getbyte(self, reg, noisy=False):
    reg = int(reg) & 0x7f
    res = self.spi.xfer([reg] + [0x0]*self.LATENCY)[self.LATENCY]
    if noisy:
      print('%s' % self.show_status(reg, res))
    return res

  def getbytes(self, reg, num, noisy=False):
    reg = int(reg) & 0x7f
    res = self.spi.xfer([reg] + [0x0]*(num + self.LATENCY - 1))[self.LATENCY:]
    if noisy:
      for val in res:
        print('%s' % self.show_status(reg, val))
    return res

  def setbit(self, reg, bit, noisy=False):
    val = self.getbyte(reg, noisy) | (1 << bit)
    self.setbyte(reg, val, noisy)
    if noisy:
      self.getbyte(reg, noisy)
    return True

  def clrbit(self, reg, bit, noisy=False):
    val = self.getbyte(reg, noisy) & ~(1 << bit)
    self.setbyte(reg, val, noisy)
    if noisy:
      self.getbyte(reg, noisy)
    return True

  def getbit(self, reg, bit, noisy=False):
    val = self.getbyte(reg, noisy) & (1 << bit)
    return val != 0


  ##------------------------------------------------------------------------##
  ##  TART system commands.
  ##------------------------------------------------------------------------##
  def reset(self, noisy=False):
    '''Issue a global reset to the TART hardware.'''
    ret = self.setbyte(self.SPI_RESET, 0x01)
    if noisy:
      print(tobin([ret]))
      print(' reset issued.')
    self.pause()
    return 1

  def status(self, noisy=False):
    '''Query all TART (SPI-mapped) registers.'''
    return self.read_status(noisy)

  def read_status(self, noisy=False):
    '''Read back the status registers of the hardware.'''
    vals = []
    for reg in self.regs:
      val = self.getbyte(reg)
      vals.append(val)
      if noisy:
        print(self.show_status(reg, val))
    return vals

  def show_status(self, reg, val):
    '''Generates a human-readable string from the given register number and contents.'''
    bits = []
    vals = []
    for b in range(8):
      vals.append(val >> b)
      bits.append(vals[b] & 0x01 == 0x01)

    msgs = {
      # Capture registers:
      self.TC_CENTRE: 'TC_CENTRE:\tcentre = %s, drift = %s, invert = %s, delay = %d' % (bits[7], bits[6], bits[5], val & 0x0f),
      self.TC_STATUS: 'TC_STATUS:\tdelta = %d, phase = %d' % (vals[4] & 0x0f, val & 0x0f),
      self.TC_DEBUG:  'TC_DEBUG: \tdebug = %s, count = %s, shift = %s, #antenna = %d' % (bits[7], bits[6], bits[5], val & 0x1f),
      self.TC_SYSTEM: 'TC_SYSTEM:\tenabled = %s, error = %s, locked = %s, source = %d' % (bits[7], bits[6], bits[5], val & 0x01f),

      # Acquisition registers:
      self.AQ_STREAM: 'AQ_STREAM:\tdata = %x' % val,
      self.AQ_SYSTEM: 'AQ_SYSTEM:\tenabled = %s, error = %s, (SDRAM) ready = %s, 512Mb = %s, overflow = %s, state = %d' % (bits[7], bits[6], bits[5], bits[4], bits[3], val & 0x07),

      # Visibilities registers:
      self.VX_STREAM: 'VX_STREAM:\tdata = %x' % val,
      self.VX_STATUS: 'VX_STATUS:\tavailable = %s, accessed = %s, overflow = %s, bank = %d' % (bits[7], bits[6], bits[5], val & 0xf),
      self.VX_DEBUG:  'VX_DEBUG: \tstuck = %s, limp = %s' % (bits[7], bits[6]),
      self.VX_SYSTEM: 'VX_SYSTEM:\tenabled = %s, overwrite = %s, blocksize = %d' % (bits[7], bits[6], val & 0x1f),

      # System register:
      self.SYS_STATS: 'SYS_STATS:\tviz_en = %s, viz_pend = %s, cap_en = %s, cap_debug = %s, acq_en = %s, state = %d' % (bits[7], bits[6], bits[5], bits[4], bits[3], val & 0x07),

      # SPI & system registers:
      self.SPI_STATS: 'SPI_STATS:\tFIFO {overflow = %s, underrun = %s}, spi_busy = %s' % (bits[7], bits[6], bits[0]),
      self.SPI_RESET: 'SPI_RESET:\treset = %s' % bits[0],

      # Miscellaneous:
      self.NEWLINE: '\r'
    }
    return msgs.get(reg, 'WARNING: Not a status register.')

  def extract(self,vals):
    unpack = lambda x: np.unpackbits(np.array([x,],dtype=np.uint8))[::-1]
    extractors = {
        self.TC_CENTRE:  lambda val: dict(zip(['centre','drift','invert','delay'], unpack(val)[[7,6,5]].tolist() + [val & 0x0f,])),
        self.TC_STATUS:  lambda val: dict(zip(['delta','phase'], [(val >>4) & 0x0f, val & 0x0f])),
        self.TC_DEBUG:   lambda val: dict(zip(['debug','count','shift','numantenna'], unpack(val)[[7,6,5]].tolist()+[val & 0x1f,])),
        self.TC_SYSTEM:  lambda val: dict(zip(['enabled','error','locked','source'], unpack(val)[[7,6,5]].tolist()+[val & 0x1f,])),

        self.AQ_STREAM:  lambda val: dict(zip(['data',], [val,])),
        self.AQ_SYSTEM:  lambda val: dict(zip(['enabled','error','SDRAM_ready','512Mb','overflow','state'],  unpack(val)[[7,6,5,4,3]].tolist()+[val & 0x07,])),

        self.VX_STREAM:  lambda val: dict(zip(['data',], [val,])),
        self.VX_STATUS:  lambda val: dict(zip(['available','accessed','overflow','bank'], unpack(val)[[7,6,5]].tolist() + [val & 0x0f,])),
        self.VX_DEBUG:   lambda val: dict(zip(['stuck','limp'], unpack(val)[[7,6]].tolist())),
        self.VX_SYSTEM:  lambda val: dict(zip(['enabled','overwrite','blocksize'], unpack(val)[[7,6]].tolist() + [val & 0x1f,])),

        self.SYS_STATS:  lambda val: dict(zip(['viz_en','viz_pend','cap_en','cap_debug','acq_en','state'], unpack(val)[[7,6,5,4,3]].tolist() + [val & 0x07,])),

        self.SPI_STATS:  lambda val: dict(zip(['FIFO_overflow','FIFO_underrun','spi_busy'],unpack(val)[[7,6,0]].tolist())),
        self.SPI_RESET:  lambda val: dict(zip(['reset',],[unpack(val)[0],])),
    }
    ret = {}
    for reg,reg_s,val in zip(self.regs, self.regs_s, vals):
        if extractors.has_key(reg):
            ret[reg_s] = extractors[reg](val)
    return ret


  ##--------------------------------------------------------------------------
  ##  Data-capture & clock-recovery settings.
  ##--------------------------------------------------------------------------
  def capture(self, on=True, source=0, noisy=False):
    '''Enable/disable the data-capture unit.'''
    if on:
      val = 0x80 | (source & 0x1f)
      flg = 'ENABLED'
    else:
      val = source & 0x1f
      flg = 'DISABLED'

    ret = self.setbyte(self.TC_SYSTEM, val)
    if noisy:
      print(' capture %s' % flg)
    self.pause()
    return ret

  def debug(self, on=True, shift=False, count=False, noisy=False):
    '''Read the debug register, and update the debug-mode flag, and then write back the new debug register value.'''
    if on:
      val = self.getbyte(self.TC_DEBUG)
      val = val | 0x80
      # Set counter mode:
      if shift:
        val = val | 0x20
      else:
        val = val & 0xdf
      if count:
        val = val | 0x40
      else:
        val = val & 0xbf
      ret = self.setbyte(self.TC_DEBUG, val)
      if noisy:
        print(' debug now ON')
    else:
      val = self.clrbit(self.TC_DEBUG, 7, noisy)
      if noisy:
        print(' debug now OFF')
    self.pause()
    return 1

  def centre(self, on=True, drift=False, invert=False, delay=0, noisy=False):
    '''Control the clock-recovery and centring unit.'''
    if on:
      val = 0x80 | (delay & 0x0f)
      if drift:
        val |= 0x40
      if invert:
        val |= 0x20
      self.setbyte(self.TC_CENTRE, val, noisy)
    else:
      self.clrbit(self.TC_CENTRE, 7, noisy)
    return True

  def signal_locked(self):
    return self.getbit(self.TC_SYSTEM, 5)

  def read_sample_delay(self, noisy=False):
    '''Read back the data sampling delays.'''
    val = self.getbyte(self.TC_CENTRE, noisy) & 0x0f
    if noisy:
      print(self.show_status(self.TC_CENTRE, val))
    self.pause()
    return val

  def set_sample_delay(self, phase=0, noisy=False):
    '''Read the sampling-delay register, and update the delay, and then write back, the new register value.'''
    if (phase < 12 and phase >= 0):
      val = self.getbyte(self.TC_CENTRE, noisy)
      val = (val & 0xf0) | (int(phase) & 0x0f)
      ret = self.setbyte(self.TC_CENTRE, val, noisy)
      self.pause()
      if noisy:
        self.getbyte(self.TC_CENTRE, True)
        # print(tobin(ret))
      return True
    else:
      if noisy:
        print('WARNING: phase value (%d) not within [0,5]' % phase)
      return False

  def read_phase_delay(self, noisy=False):
    '''Read back the current signal/antenna phase-delay.'''
    ret = self.getbyte(self.TC_STATUS, noisy)
    val = ret & 0x0f
    if noisy:
      print(tobin([ret]))
    self.pause()
    return val

  def read_phases(self, num, noisy=False):
    '''Read back the multiple signal/antenna phase-delays.'''
    ret = self.getbytes(self.TC_STATUS, num, noisy)
    res = []
    for val in ret:
      res.append(val & 0x0f)
    if noisy:
      print(tobin(res))
    self.pause()
    return res


  ##------------------------------------------------------------------------##
  ##  Data acquisition settings, and streaming read-back.
  ##------------------------------------------------------------------------##
  def start_acquisition(self, sleeptime=0.2, noisy=False):
    '''Enable the data-acquisition flag, and then read back the acquisition-status register, to verify that acquisition has begun.'''
    old = self.getbyte(self.AQ_SYSTEM)
    ret = self.setbyte(self.AQ_SYSTEM, old | 0x80)
    val = self.getbit (self.AQ_SYSTEM, 7)
    if noisy:
      print(' attempting to set acquisition-mode to ON')
      print(tobin([old]))
      print(tobin([ret]))
    if val:
      self.pause(sleeptime)       # optionally wait for acquisition to end
      res = self.getbyte(self.AQ_SYSTEM)
      if noisy:
        print(self.show_status(self.AQ_SYSTEM, res))
      fin = res & 0x07 > 2
    self.pause()
    return val and fin

  def read_data(self, num_words=2**21, blocksize=1024):
    '''Read back the requested number of 24-bit words.'''
    # Determine the number of blocks to transfer, and the remainder for the
    # final transfer.
    blk = int(num_words / blocksize)
    lst = int(num_words % blocksize)
    # Stream back the raw acquisition data.
    dat = []
    for i in range(0, blk):
      dat += self.getbytes(self.AQ_STREAM, blocksize*3)
    dat += self.getbytes(self.AQ_STREAM, lst*3)
    # Convert to a
    dat = np.array(dat, dtype=np.uint32).reshape(-1,3)
    return dat

  def data_ready(self):
    '''Check the system register, of the acquistion unit, to see if the data is ready.'''
    val = self.getbyte(self.AQ_SYSTEM) & 0x07
    return val > 2


  ##--------------------------------------------------------------------------
  ##  Visibility-calculation settings, and streaming read-back.
  ##--------------------------------------------------------------------------
  def start(self, blocksize=24, noisy=False):
#     self.set_blocksize_and_start(blocksize, overwrite=True, noisy)
    self.set_blocksize_and_start(blocksize, overwrite=False, noisy=noisy)
#     self.start_acquisition(noisy)

  def set_blocksize_and_start(self, blocksize=24, overwrite=False, noisy=False):
    '''Set the correlator blocksize to 2^blocksize.\nNOTE: With `overwrite` enabled, the TART device can get itself into invalid states.'''
    if blocksize > 9 and blocksize < 25:
      if overwrite:
        ow = 0x40
      else:
        ow = 0x00
      bs  = 0x80 | ow | int(blocksize)
      self.blocksize = blocksize
      ret = self.spi.xfer([self.WRITE_CMD | self.VX_SYSTEM, bs])
      self.pause()
      if noisy:
        print(tobin(ret))
      return 1
    else:
      return 0

  def get_blocksize(self, noisy=False):
    '''Get the (exponent of the) correlator block-size.'''
    ret = self.getbyte(self.VX_SYSTEM)
    if noisy:
      print(self.show_status(self.VX_SYSTEM,ret))
    return ret & 0x1f

  def read_visibilities(self, noisy=True):
    '''Read back visibilities data.'''
    res = self.getbytes(self.VX_STREAM, 4*576)
    val = self.vis_convert(res)
    if noisy:
      tim = time.time()
      print(" Visibilities (@t = %g):\n%s (sum = %d)" % (tim, val[self.perm]-int(2**(self.blocksize-1)), sum(val)))
    return val

  def vis_ready(self, noisy=False):
    rdy = self.getbit(self.VX_STATUS, 7)
    if noisy:
      print('\tready = %s' % rdy)
    return rdy

  def vis_read(self, noisy=False):
    while not self.vis_ready(noisy):
      self.pause()
    vis = self.read_visibilities(noisy)
    return vis

  def vis_convert(self, viz):
    arr = np.zeros(576, dtype='int')
    for i in range(0,576):
      j = i*4
      x = viz[j] | (viz[j+1] << 8) | (viz[j+2] << 16) | ((viz[j+3] & 0x7f) << 24)
      if viz[j+3] > 0x7f:
        x = -x
      arr[i] = x
    return arr

  def load_permute(self, filepath=PERMUTE_FILE, noisy=False):
    '''Load a permutation vector from the file at the given filepath.'''
    if self.perm is None:
      pp = np.loadtxt(filepath, dtype='int')
      self.perm = pp
    return self.perm

#endclass TartSPI
