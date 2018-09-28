# Gain/Phase Calibration of the TART telescope

Author: Tim Molteno tim@elec.ac.nz (c) 2017-2018

Once the positions of the antennas are known, then phase calibration is possible from observing the measured phases from a known source position.

**NOTE: Every time the telescope is powered off, the phase calibration will need to be redone**


## Automatic Calibration from Satellite Positions

A software container that will automatically do this (and install the required pre-requisites) is described in [software/containers/calibration_server](../../../software/containers/calibration_server/README.md)


## Manual Calibration

The remainder of this README will describe how to do manual calibration. Work through an example.

#### Scenario.

The TART is configured to:

* Use a single tile with 6 receivers. These are numbered 0...5. 
* The antenna positions have been measured and recorded as described [here](../positions/README.md)
* A noise source is placed at a known position relative to the telescope (x,y,z).

Note. The coordinates will all be with x,y,z being north, east and vertical respectively from the telescope center in meters.

Step 1. The calibration needs to know the 

    TART_API=http://your.tart.ip.address:xxxx
    WORKING_DIR=/app
    METHOD=BH
    DATESTR=`date "+%Y_%m_%d_%H_%M_%S"`
    DIR=${WORKING_DIR}/calibration_${DATESTR}
    mkdir -p ${DIR}

    CALIB_INPUT=${DIR}/calib_data.json
    CALIB_OUTPUT=${DIR}/${METHOD}_opt_json.json

    # Get calibration data
    /usr/local/bin/tart_calibration_data --api ${TART_API} --n 3 --i 25 --file ${CALIB_INPUT}

    # Perform optimization
    /usr/local/bin/tart_calibrate --api ${TART_API} --file ${CALIB_INPUT} --method ${METHOD} --dir ${DIR}

    # Log outputs
    LOGGED_OUTPUT=${WORKING_DIR}/cal_${DATESTR}.json
    mv ${CALIB_OUTPUT} ${LOGGED_OUTPUT}
    echo "Calibration output is in ${LOGGED_OUTPUT}"
    /usr/local/bin/tart_upload_gains --api ${TART_API} --gains ${LOGGED_OUTPUT} --pw ${TART_LOGIN_PW}
