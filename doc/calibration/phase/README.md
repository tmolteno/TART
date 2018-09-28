# Gain/Phase Calibration of the TART telescope

Author: Tim Molteno tim@elec.ac.nz (c) 2017-2018

Script tart_calibrate.sh should be added as a cron job on a reasonably powerful machine, and run every two hours.

    # m h  dom mon dow   command
    15 */2 * * * cd /home/tim/github/TART/doc/calibration/phase/; sh tart_calibrate.sh

A software container that will automatically do this (and install the required pre-requisites) is described in [software/containers/calibration_server](../../../software/containers/calibration_server/README.md)
