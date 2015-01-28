#!/bin/sh
# Create local data directory and change owner to pi:
#    sudo mkdir /data
#    sudo chown pi:pi /data

# Copy data to the remote server and delete the local copy after successful transfer
#
# The user running this script will have to have done an SSH key exchange with the remote
# server. 
#    ssh-keygen 
#    ssh-copyid ${REMOTE}
# Now add to crontab
# 5 * * * * sh /home/pi/git/TART/hardware/rpi/copy_data_remote.sh
#
REMOTE=tart@electron.otago.ac.nz
HOST=`hostname`
rsync --recursive --remove-source-files /data/* ${REMOTE}:/freenas2/tart/data/${HOST}/ 2>&1
