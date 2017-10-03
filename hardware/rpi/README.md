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
    sudo chown -R pi:pi /data
    sudo mkdir -p /var/www/html/assets/img/
    sudo mkdir -p /var/www/html/doc
    sudo chown -R pi:pi /var/www/html/
```
## Create RAM disc for raw data storage to avoid SDCARD writes
```
    sudo su
    echo 'tmpfs	/var/www/html/raw	tmpfs	size=200M,noatime	0 0' >> /etc/fstab
    echo 'tmpfs	/var/www/html/vis	tmpfs	size=100M,noatime	0 0' >> /etc/fstab
    sudo reboot
```

## Update to latest software
```
    sudo apt-get update
    sudo apt-get dist-upgrade
    sudo apt-get install aptitude

    sudo apt-get install screen python-setuptools ntp python-dev autossh git
    sudo apt-get install python-yaml python-scipy python-astropy python-psycopg2 python-setuptools ntp python-dev autossh git python-jsonrpclib
    sudo pip install healpy

    curl -sL https://deb.nodesource.com/setup_8.x | sudo -E bash -
    sudo apt-get install -y nodejs
    sudo apt-get install npm 
    sudo npm cache clean -f
    sudo npm install -g n
    sudo n stable
    sudo npm install apidoc -g
```


## Install TART python packages, hardware_interface and telescope API
```
   sudo pip install tart tart_hardware_interface tart_web_api
```
## (Optional) Clone Github TART project repository

```
    cd ~/
    mkdir git
    cd ~/git
    git clone https://github.com/tmolteno/TART.git
```

### Install NGINX to serve web frontend
```
    sudo aptitude install nginx
```

### Configure NGINX (e.g.: for an API endpoint called 'lab')
Edit /etc/nginx/nginx.conf
```
http {
        add_header Access-Control-Allow-Origin *;
        gzip_vary on;
        gzip_proxied any;
        gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript;

```
Edit /etc/nginx/sites-available/default
```
	...
        server_name _;

        location /api/v1 {
                rewrite ^/api/v1(.*) $1 break;
                proxy_pass http://127.0.0.1:5000/;
        }

        location /lab/api/v1 {
                rewrite ^/lab/api/v1(.*) $1 break;
                proxy_pass http://127.0.0.1:5000/;
        }
        location /lab {
                rewrite ^/lab(.*) $1 break;
                try_files $uri $uri/ /index.html;
        }


        location / {
	...
```


#### Run telescope API
From within TART repo ~/git/TART/hardware/rpi:
```
    make
```


# Working Remotely with the TART

After login, you can kill the running process with

sudo killall python flask

You can run it with
```
    screen
    export FLASK_APP=tart_web_api
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


## RAW Data storage format
Currently data is stored as pickled observation objects in files using a directory structure that
consists of yyyy/mm/dd/h_m_s.pkl. See the Observation object for more details

```
    ls /var/www/html/raw/yyyy/mm/dd/h_m_s.pkl
```


## Autostart API and start SSH tunnels and Network setup
```
    ssh-keygen
    ssh-copy-id tart@tart.elec.ac.nz
```

Replace ports 80XX and 30XX with assigned ports from tim@elec.ac.nz
Edit /etc/rc.local

```
su pi -c 'export AUTOSSH_LOGFILE="/home/pi/autossh.log"; autossh -M 0 -f -N -T -q -i /home/pi/.ssh/id_rsa -o ExitOnForwardFailure=yes -o ServerAliveInterval=10 -o ServerAliveCountMax=1 -R 80XX:localhost:80 -R 30XX:localhost:22 tart@tart.elec.ac.nz' &

su pi -c 'cd /home/pi/git/TART/hardware/rpi && make'
```

## Connecting to TART remotely

Each TART node uses autossh to forward a port on a remote server to ssh on the local machine.
Each TART node forwards a DIFFERENT port.

If a TART node were forwarding 3022, then after logging in to the remote machine (tart.elec.ac.nz)
the following commands would connect back to the TART (wherever it was in the world)
```
ssh -p 3022 pi@localhost
```


### (Optional for developer) Install tart software package
```
    cd ~/git/TART/software/python/tart
    sudo python setup.py develop
```


### (Optional for developer) Install SPI driver communication with FPGA
```
    sudo easy_install --upgrade pip
    cd ~/git/TART/software/python/tart_hardware_interface
    sudo python setup.py develop
```

### (Optional for developer) Install telescope API and APIDOC
```
    cd ~/git/TART/software/python/tart_web_api/
    sudo python setup.py develop
    cd tart_web_api
    make  # requires apidoc to be installed
```


