#!/usr/bin/python
#
# Copyright (c) 2013-2014 Tim Molteno, tim@elec.ac.nz
#
import spi

# The CS pin is connected to CS0 on SPI port of the rPi.

import RPi.GPIO as GPIO
# The RESET pin is connected to GPIO 22 on the rPi
import numpy as np
import time

'''This class encapsulates interaction with the TART over the SPI interface


'''
class TartSPI:
  
  # Registers
  ADC_DATA_CH0_B2=0x00
  ADC_DATA_CH0_B1=0x01
  ADC_DATA_CH0_B0=0x02

  ADC_DATA_CH1_B2=0x03
  ADC_DATA_CH1_B1=0x04
  ADC_DATA_CH1_B0=0x05

  MOD=0x06 # Delta-sigma modulators output
  PHASE=0x07 # Phase delay
  GAIN=0x08 # Gain

  STATUS=0x09
  CONFIG1=0x0A
  CONFIG2=0x0B


  def __init__(self, reset_pin, gain):
    self.status = spi.openSPI(mode=0,speed=4000000,bits=8)
    print "SPI configurations = ", self.status
    
    MCP3901.reset(reset_pin)
    self.MCLK = 4000000
    self.PRESCALE = 8
    self.AMCLK = self.MCLK / self.PRESCALE
    self.DMCLK = self.AMCLK / 4 # Digital Master Clock
    self.OSR = 256 # Oversampling Ratio
    self.DRCLK = self.DMCLK / self.OSR # Data Rate
    print "Data Rate %d" % self.DRCLK
    
    rst =     0b00000011 # Both channel in reset mode
    shutdown= 0b00000011 # Both channels in shutdown mode
    dither =  0b00000011 # Both channels have dithering applied
    vrefext=  0b00000000 #Internal voltage reference enabled
    clkext=   0b00000000 # XT Mode XTAL 4MHz
    config2_data =  ((rst << 6) | (shutdown << 4) | (dither << 2) | (vrefext << 1) | clkext)
    self.reg_write(MCP3901.CONFIG2, config2_data)
    
    # See the recommended RESET sequence on Page 37 (bottom diagram)
    
    pre    = 0b00000011
    osr    = 0b00000011 # Oversampling of 256
    width  = 0b00000011 # 24-bit mode in each channel
    modout = 0b00000000 # No modulator output
    config1_data = (pre << 6) | (osr << 4) | (width << 2) | (modout)

    # Enable both channels
    rst =     0b00000000 # Neither channel in reset mode
    shutdown= 0b00000000 # Neither channel in shutdown mode
    config2_data =  ((rst << 6) | (shutdown << 4) | (dither << 2) | (vrefext << 1) | clkext)

    mod_data = 0
    phase_data = 0
    gain_data = self.get_gain_register(gain=gain)
    status_data = self.get_status_register(read=0b10)
    # Write each register one at a time
    if (False):
      self.reg_write(MCP3901.PHASE, phase_data)
      self.reg_write(MCP3901.GAIN, self.get_gain_register(gain=gain))
      
      self.reg_write(MCP3901.STATUS, self.get_status_register(read=0b10))
      self.reg_write(MCP3901.CONFIG1, config1_data)
      self.reg_write(MCP3901.CONFIG2, config2_data)
    else:
      x = spi.transfer((self.write_control_word(MCP3901.MOD), mod_data, phase_data, gain_data, status_data, config1_data, config2_data))
      print x
      
    print "Status registers: ",
    print self.reg_read6(0x00), 
    print self.reg_read6(0x06)

  def close(self):
    spi.closeSPI()
    
  @classmethod
  def reset(self, reset_pin):
    # Do a reset.
    GPIO.setmode(GPIO.BCM)
    GPIO.setup(reset_pin, GPIO.OUT, initial=GPIO.HIGH) 
    time.sleep(0.1)
    GPIO.output(reset_pin, GPIO.LOW)
    time.sleep(0.1)
    GPIO.output(reset_pin, GPIO.HIGH)
  
  # First byte is a CONTROL byte followed by 8-bit wide data bytes.
  # [0 0 A4 A3 A2 A1 A0 RW]
  def read_control_word(self, reg):
    return (reg << 1) | 0b00000001

  def write_control_word(self, reg):
    return (reg << 1)

  def get_gain_register(self, gain=1):
    if (gain == 1):
      ch1=0b000
    if (gain == 2):
      ch1=0b001
    if (gain == 4):
      ch1=0b010
    if (gain == 8):
      ch1=0b011
    if (gain == 16):
      ch1=0b100
    if (gain == 32):
      ch1=0b101
    boost=0b00000000
    return ((ch1 << 5) | (boost << 3) | ch1)

  def get_status_register(self, read, dr_lty=0b1, dr_hizn=0b0, drmode=0b00, drstatus_ch=0b11):
    return ((read << 6) | (dr_lty << 5) | (dr_hizn << 4) | (drmode << 2) | drstatus_ch)

  def reg_write(self, reg, value):
    spi.transfer((self.write_control_word(reg), value & 0xFF))
    
  def reg_read(self, reg):
    x = spi.transfer((self.read_control_word(reg), 0))
    return x[1]

  def reg_read6(self, reg):
    x = spi.transfer((self.read_control_word(reg), 0,0,0,0,0,0))
    return x[1], x[2], x[3], x[4], x[5], x[6]

  def wait_for_data(self):
    while (self.reg_read(MCP3901.STATUS) & 0b00000011) != 0: # These bits are cleared when data is ready
      pass
  
  def convert(self, b2, b1, b0):
    ret = (b2 << 16) | (b1 << 8) | b0
    if ret > 2**23:
      ret = ret - 0xFFFFFF
    return ret
    
  def read_ch0(self):
    b2 = self.reg_read(self.ADC_DATA_CH0_B2)
    b1 = self.reg_read(self.ADC_DATA_CH0_B1)
    b0 = self.reg_read(self.ADC_DATA_CH0_B0)
    return self.convert(b2, b1, b0)

  def read_ch1(self):
    b2 = self.reg_read(self.ADC_DATA_CH1_B2)
    b1 = self.reg_read(self.ADC_DATA_CH1_B1)
    b0 = self.reg_read(self.ADC_DATA_CH1_B0)
    return self.convert(b2, b1, b0)

  def read_both(self):
    #self.wait_for_data()
    c1b2, c1b1, c1b0, c2b2, c2b1, c2b0 = self.reg_read6(0x00)
    return (self.convert(c1b2, c1b1, c1b0), self.convert(c2b2, c2b1, c2b0))


  def read_vector(self, n):
    # Set up data transfer
    self.reg_write(MCP3901.STATUS, self.get_status_register(read=0b00))
    ret = np.zeros(n)
    for i, x in enumerate(ret):
      self.wait_for_data()
      ret[i] = self.read_ch0()
    return ret

if __name__ == '__main__':
  import time
  import argparse
  parser = argparse.ArgumentParser(description='Acquire data from TART.')
  parser.add_argument('--data-directory', default='.', help="The filesystem path for the downloaded data.")
  parser.add_argument('--monitor', type=int, default=1, help="Monitor the Load Cells")

  args = parser.parse_args()
  print args
  base_path = args.data_directory
  mon = args.monitor
  
  tart = TartSPI(reset_pin=22, gain=16)
  if mon == 1:
    while True:
	d0, d1 = tart.read_both()
	print "%8d \t %8d\r" % (d0, d1),
	  

  vec = tart.read_vector(10000)
  print vec.mean()
  print vec.std()


