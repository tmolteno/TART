# TART Operating Software

The basic code is going to read data from the TART data on the SPI port, 
and writes it to a folder on the RPi.

A separate cron job rsync's the data files to the remote host for processing.


## Working Remotely with the TART

After login, you can kill the running process with

    sudo killall python

You can run it with

    screen sh startup.sh

You can look at the output with

    screen -r

###Using screen

To exit the screen

   CTRL-A CTRL-D


## Data storage formats

Currently data is stored as pickled measurement objects in files using a directory structure that 
consists of yyyy/mm/dd/h_m_s.pkl. See the measurement object for more details

## Connecting to TART remotely

Each TART node uses autossh to forward a port on a remote server to ssh on the local machine.
Each TART node forwards a DIFFERENT port.

If a TART node were forwarding 2222, then after logging in to the remote machine (tart.elec.ac.nz)
the following commands would connect back to the TART (wherever it was in the world)

    ssh -p 2222 localhost

#Machine Setup

Update to latest firmware.

    sudo apt-get update
    sudo apt-get dist-upgrade 
    sudo aptitude install python-setuptools ntp

    git clone https://github.com/tmolteno/TART.git
    cd TART/python
    sudo python setup.py install

##  SPI

    sudo aptitude install python-dev autossh

    git clone https://github.com/lthiery/SPI-Py.git
    cd SPI-Py
    sudo python setup.py install

## Network setup

Maintain a tunnel between the pi and tart.elec.ac.nz. We will choose a port to map. 
THIS PORT MUST BE UNIQUE. In this example we use 2222. You should choose another number.
The tim@tart.elec.ac.nz is the usual one.

    ssh-keygen
    ssh-copy-id tim@tart.elec.ac.nz

Add the following to the Pi /etc/rc.local

    su pi -c 'autossh -N -f -M 29001 -R 2222:localhost:22 tim@tart.elec.ac.nz' &

Confirm that the TART is connecting to the remote host by 

    ssh tim@tart.elec.ac.nz

and then issuing the following command

    ssh -p 2222 pi@localhost

And you should now be connected to the tag. This can be done in the field using the 
JuiceSSH client on an android phone.

##Install 3G Dongle

We use the Huawei HiLink 3G modems, and to install use the instuctions at:

  http://techmind.org/rpi/
  
While still logged into the pi:

    sudo apt-get install sg3-utils
    sudo reboot
    ssh -p 2222 pi@localhost
  
Set the mode of the Huawei modem:

    sudo /usr/bin/sg_raw /dev/sr0 11 06 20 00 00 00 00 00 01 00
  
You should get back:

    SCSI Status: Good
        
    Sense Information:
    sense buffer empty

Set an address on the new eth1:

    ifconfig eth1 192.168.1.10
  
The Huawei address is set in firmware as 192.168.1.1

To automate the mode switch, create a new file /etc/udev/rules.d/10-Huawei.rules

    sudo nano /etc/udev/rules.d/10-Huawei.rules

and paste in the following content:

    SUBSYSTEMS=="usb", ATTRS{modalias}=="usb:v12D1p1F01*", SYMLINK+="hwcdrom", RUN+="/usr/bin/sg_raw /dev/hwcdrom 11 06 20 00 00 00 00 00 01 00"

Save the modified file.

Next customise the /etc/network/interfaces configuration file:

    sudo nano /etc/network/interfaces

Paste at the end of the file:

    allow-hotplug eth1
    iface eth1 inet dhcp

Save the modified file.

Reboot the Pi:

    sudo reboot

The Pi should now say "My IP address is xxx.xxx.xxx.xxx 192.168.1.100", where xxx.xxx.xxx.xxx = the IP address of the RJ45 ethernet, and 192.168.1.100 is the address assigned to the Pi by the Huawei.



## Automatically run at startup

First log the user pi in at startup.

    sudo nano /etc/inittab
    1:2345:respawn:/bin/login -f pi tty1 </dev/tty1 >/dev/tty1 2>&1

To start up the TART software create the following script  (startup.sh) in the home directory

    cp /home/pi/TART/rpi/software/startup.sh /home/pi/startup.sh

Add the following to ~/.bashrc

    # Run the startup bash script:
    echo .bashrc : Running startup.sh
    bash startup.sh

## Data sync script

Add the following line to crontab:

    crontab -e
    5 * * * * sh /home/pi/TART/rpi/software/copy_data_remote.sh
  
Add SSH credentials to electron (where the data will be sent),
the password is the usual.

    ssh-copy-id tart@electron.otago.ac.nz
  
Login to electron to check:

    ssh tart@electron.otago.ac.nz
  
Test the file saving script:

    sh ~/TART/rpi/software/copy_data_remote.sh
  
If it goes you are done.
