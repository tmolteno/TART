#!/bin/sh
#
# Continuous Calibration Script.
#
# Tim Molteno (tim@elec.ac.nz)
#
# See the paper Molteno et al. "Continuous Calibration of the Transient Array Radio Telescope using Satellites" from ENZCon 2017.
#
# This should be run at regular intervals.
# sh tart_calibrate.sh > calibration.log
#
# NOTE. Modify the <password> to be the password for your telescope to allow the gains
# and phases to be saved.
#
METHOD=BH
DATESTR=`date "+%Y_%m_%d_%H_%M_%S"`
API=https://tart.elec.ac.nz/signal/
DIR=calibration_${DATESTR}
mkdir -p ${DIR}

CALIB_INPUT=${DIR}/calib_data.json
CALIB_OUTPUT=${DIR}/${METHOD}_opt_json.json

# Get calibration data
/usr/local/bin/tart_calibration_data --api ${API} --n 3 --i 25 --file ${CALIB_INPUT}

# Perform optimization
/usr/local/bin/tart_calibrate --api ${API} --file ${CALIB_INPUT} --method ${METHOD} --dir ${DIR}

# Log outputs
LOGGED_OUTPUT=cal_${DATESTR}.json
mv ${CALIB_OUTPUT} ${LOGGED_OUTPUT}
echo "Calibration output is in ${LOGGED_OUTPUT}"
/usr/local/bin/tart_upload_gains --api ${API} --gains ${LOGGED_OUTPUT} --pw <password>

# Clean up
rm ${DIR}/*
rmdir ${DIR}
