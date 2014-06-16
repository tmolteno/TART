#!/usr/bin/python
# Copyright (c) Tim Molteno, tim@elec.ac.nz. 2013.
import numpy as np

from mcp3901 import MCP3901


if __name__ == '__main__':
  import time
  import argparse
  parser = argparse.ArgumentParser(description='Acquire data from SAuSI WoW.')
  parser.add_argument('--data-directory', default='.', help="The filesystem path for the downloaded data.")
  parser.add_argument('--monitor', type=int, default=1, help="Monitor the Load Cells")

  args = parser.parse_args()
  print args
  base_path = args.data_directory
  mon = args.monitor
  
  adc = MCP3901(reset_pin=22, gain=16);
  if mon == 1:
    while True:
	d0, d1 = adc.read_both()
	print "%8d \t %8d\r" % (d0, d1),
	  

  vec = adc.read_vector(10000)
  print vec.mean()
  print vec.std()
  adc.close()

