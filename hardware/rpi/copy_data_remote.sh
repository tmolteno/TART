#!/bin/sh
#
# Copy data to the remote server and delete the local copy after successful transfer
#
# The user running this script will have to have done an SSH key exchange with the remote
# server. 
#    ssh-keygen 
#    ssh-copyid ${REMOTE}
# Now add to crontab
# 5 * * * * sh /home/tim/WoW/rpi/software/copy_data_remote.sh
#
REMOTE=wow@electron.otago.ac.nz
HOST=`hostname`
rsync --recursive --remove-source-files /freenas/cow/data/* ${REMOTE}:/freenas/cow/data/${HOST}/ 2>&1
