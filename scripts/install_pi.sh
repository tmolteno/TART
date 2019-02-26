#!/bin/sh
#
#   Script to install a TART telescope on a raspberry-pi using docker.
#   this script is handy as it just uploads the differences so you can run this regularly
#
#   You can avoid typing the password on your pi with the command:
#       ssh-copy-id pi@tart2-dev
#   this copies the local SSH keys to the remote telescope.
#
#   Author: Tim Molteno tim@elec.ac.nz (c) 2018-2019
#
TARGET=pi@tart2-dev


rsync -rv --exclude=node_modules ../software ${TARGET}:.

# Then install docker and docker-compose on the target raspberry pi
# 
# curl -fsSL get.docker.com -o get-docker.sh && sh get-docker.sh
# sudo gpasswd -a $USER docker
# sudo pip install docker-compose

# Now log into the pi and run:
#   cd software
#   docker-compose build
#   docker-compose up
# Point your browser to the raspberry pi.
