# TART Operating Software

The basic code is going to read data from the TART data on the SPI port,
and writes it to a folder on the RPi.

A separate cron job rsync's the data files to the remote host for processing.


# Machine Setup

## Download latest raspbian jessie image and dd it onto an micro sdcard
```
    sudo dd bs=1M im=rasp.img of=/dev/SDCARD
```
This will take a couple of minutes...
When done boot up raspberry pi.
```
    user: pi
    pw: raspberry
```
## Login and change pw and root pw:
```
    passwd
    sudo su
    passwd
```
## Set hostname, activate SPI & SSH:
```
    sudo raspi-config
```
## Create directories for data and web frontend
```
    sudo mkdir /data
    chown -R pi:pi /data
    sudo mkdir -p /var/www/html/assets/img/
    chown -R pi:pi /var/www/html/
```
## Update to latest firmware
```
    sudo apt-get update
    sudo apt-get dist-upgrade
    sudo apt-get aptitude

    sudo aptitude install python-setuptools ntp python-dev autossh git
```

## Clone TART project repository
```
    git clone https://github.com/tmolteno/TART.git
    cd TART/python
    sudo python setup.py develop
```

### Install NGINX to serve web frontend
```
    sudo aptitude install nginx
```

### Install SPI driver communication with FPGA
```
    cd tart_dsp
    sudo python setup.py develop
```

### Install telescope API
```
    cd telescope_api
    sudo python setup.py develop
```

#### Run telescope API
```
    export FLASK_APP=telescope_api
    flask run -h 0.0.0.0 -p 5000
```



# Working Remotely with the TART

After login, you can kill the running process with

sudo killall python flask

You can run it with
```
    screen
    export FLASK_APP=telescope_api
    flask run -h 0.0.0.0 -p 5000
```
You can look at the output with
```
    screen -r
```
### Using screen

To exit the screen
```
    CTRL-A CTRL-D
```


## Data storage formats
Currently data is stored as pickled measurement objects in files using a directory structure that
consists of yyyy/mm/dd/h_m_s.pkl. See the measurement object for more details

```
    ls /data
```


## Connecting to TART remotely

Each TART node uses autossh to forward a port on a remote server to ssh on the local machine.
Each TART node forwards a DIFFERENT port.

If a TART node were forwarding 2222, then after logging in to the remote machine (tart.elec.ac.nz)
the following commands would connect back to the TART (wherever it was in the world)

ssh -p 2222 localhost


## Network setup

TBA

    ssh-keygen
    ssh-copy-id tim@tart.elec.ac.nz

## Data sync script

Add the following line to crontab:

    crontab -e
    5 * * * * sh /home/pi/TART/rpi/software/copy_data_remote.sh

Add SSH credentials to electron (where the data will be sent),
the password is the usual.

    ssh-copy-id tart@electron.otago.ac.nz

Login to electron to check:

    ssh tart@electron.otago.ac.nz

