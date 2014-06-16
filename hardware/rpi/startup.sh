#!/bin/bash
# Start the TART software
#
if [ "$(pidof python)" ] 
then
  echo "TART process is already running. Use killall python to stop it."
else
  sudo modprobe spi_bcm2708
  echo startup.sh : Launching TART Software
  cd TART/rpi/software; sudo python interrupt_loop.py 
fi
