# Copyright 2013-2014 Electronics Research
# Author: Tim Molteno tim@elec.ac.nz
# 
# Set up the buffer for force measurements and sample at regular intervals into these.
# Also set up an interrupt for the RS232 input from the EID reader. These are also
# buffered.
#
# A Measurement object is created from this buffered data when a suitable condition is
# satisfied. This condition is triggered if the is_measurement_ready() function returns True
from tart_spi import TartSPI
import serial
import io
import sys
import copy
import json

# RPi GPIO support
import RPi.GPIO as GPIO
import datetime
import numpy as np


from multiprocessing import Process, Queue


class DataBuffers:
  def __init__(self, eid_queue, eid_in_port, eid_out_port ):
    # This class contains all the buffers for measurements
    self.eid_queue = eid_queue
    self.measurements = {}
    
    self.rate = 488
    self.sec = 10.0
    self.extra_sec = 3.0
    
    self.f0_zero = 0.0
    self.f1_zero = 0.0
    self.clear_stats()
    self.f_ptr = 0
    self.buffer_size = int(self.rate*self.sec)
    self.f0 = np.zeros(self.buffer_size, dtype=np.float32)
    self.f1 = np.zeros(self.buffer_size, dtype=np.float32)
    # Now setup interrupts
    print GPIO.VERSION

    GPIO.setmode(GPIO.BCM) # We use Broaccom port numbering  
    GPIO.setwarnings(True)
    self.tart = TartSPI(reset_pin=22, gain=16)
    print "TART SPI initialized" 
    # The DR pin must be connected to one of the GPIO ports
    # This is triggerred 488 times per second (in the slowest
    # mode of the MCP3901 ADC chip
    
    # GPIO 25 set up as inputs, pulled up to avoid false detection.  
    # Both ports are wired to connect to GND on button press.  
    # So we'll be setting up falling edge detection for both  
    GPIO.setup(25, GPIO.IN, pull_up_down=GPIO.PUD_UP)  
    # when a falling edge is detected on port 17, regardless of whatever   
    # else is happening in the program, the function data_ready_callback will be run  
    GPIO.add_event_detect(25, GPIO.FALLING, callback=self.data_ready_callback, bouncetime=0)  


  def clear_stats(self):
    self.f0_sum = 0.0
    self.f1_sum = 0.0

    self.stat_n = 0

    
  def is_measurement_ready(self):
    # If the eids have changed
    for eid in self.measurements.keys():
      m = self.measurements[eid]
      t = datetime.datetime.now()
      data_expired = ((t - m.eids[-1][1]).total_seconds() > self.extra_sec)

      if (data_expired):
        fp1 = self.f_ptr+1
        n = self.buffer_size
        decimate = 5
        f0, f1 = self.scaled_data()
        m.f0 = np.concatenate((f0[fp1:n], f0[0:fp1]))[0::decimate]
        m.f1 = np.concatenate((f1[fp1:n], f1[0:fp1]))[0::decimate]
        m.num_samples = len(m.f0)
        m.sampling_rate = self.rate / decimate
        m.timestamp = t # The time of measurement is the time of the last sample
        return m
      else:
        return False
    else:
      return False

  # This function will run in another thread when our events are detected 
  # This will store the TART data into local memory np.array
  def data_ready_callback(self, channel):  
      #pddrint "falling edge detected on DR"
      self.f_ptr = (self.f_ptr + 1) % self.buffer_size
      d0, d1 = self.tart.read_both()
      self.f0[self.f_ptr] = d0
      self.f1[self.f_ptr] = d1
  

  def scaled_data(self):
    # 80kg corresponds to 40,000
    scaling = 80.0 / 80000.0
    # Zero offset
    f0 = (self.f0 - self.f0_zero)*scaling
    f1 = (self.f1 - self.f1_zero)*scaling
    return f0, f1
  
  def callibrate(self):
    if ((self.f0.mean() < (self.f0_zero + 2000)) and (self.f1.mean() < (self.f1_zero + 2000.0))):
      if (self.f0.std() < 1000.0):
        if (self.stat_n > 10):
          self.clear_stats()
        self.f0_sum += self.f0.mean()
        self.f1_sum += self.f1.mean()
        self.stat_n += 1

        self.f0_zero = self.f0_sum / self.stat_n
        self.f1_zero = self.f1_sum / self.stat_n
      
import image_capture
import time
import argparse

if __name__ == '__main__':
  parser = argparse.ArgumentParser(description='Plot measurements.')
  parser.add_argument('--data-dir', default="/freenas/TART/data", help="The root directory for data storage.")

  args = parser.parse_args()

  data_dir = args.data_dir
  
  while (True):
    # Sleep for 0.1 seconds or so
    dt = 0.02
    time.sleep(dt)
    n = n + 1
    db.check_for_eid()
    m = db.is_measurement_ready()
    if (m != False):
      try:
        full_path = m.get_path(data_dir)
        print "Saving Measurement %s" % m.eids[0]
        m.save(full_path)
      except Exception, e:
        print "Could not save measurement: %s" % str(e)
      del db.measurements[m.tag_id]
        
      #image_capture.capture(full_path)
    elif (n%int(db.sec/dt) == 0):
      f0, f1 = db.scaled_data()
      print("%7.2f,\t%7.2f,\t%7.2f,\t%7.2f" % (f0.mean(), f0.max(), f1.mean(), f1.max()))
      db.callibrate()
    
  GPIO.cleanup()           # clean up GPIO on normal exit  
