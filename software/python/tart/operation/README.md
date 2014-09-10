#Python Code


##System Requirements for Acquisition

  aptitude install python-serial python-pyfits
  
##Normal Operation

The program is designed to be run by a scheduler. However for testing, use the following commands

    python sig_acquire.py

This will open the serial port, initiate sampling at FPGA and save data. The data is saved in a timestamped file in a directory structure YYYY/mm/dd/<timestamp>

##Data format

The data is stored by pickling an Observation object in binary format. Have a look at data_info.py
for example code.

##Looking at Data

THere is a function called data_info.py that will open up a data file and calculate
the visibility. To run it on all the files for a particular day, use

    find /freenas/telescope/data/2013/3/19/ -name *.pkl -exec ./data_info.py {} \; | grep Visibility

To monitor the most recent acquisition, use the unix watch command

    watch -n 60 './data_info.py --file `ls /freenas/telescope/data/2013/3/19/*.pkl | tail -1`' 

To plot the spectrum
    ./data_info.py --file /freenas/telescope/data/2013/3/19/foo.pkl --plot 1
  

