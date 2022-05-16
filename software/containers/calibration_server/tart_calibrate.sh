#!/bin/sh
#
# Continuous Calibration Script. This should be run at regular intervals.
#
# Author Tim Molteno. tim@elec.ac.nz
#
# Reference: Molteno et al. Continuous Calibration of the TART using GPS satellites. EnzCon 2017.
#
WORKING_DIR=/app
TART_CAL_METHOD=BH # DE is really quick but does not produce good results.
DATESTR=`date "+%Y_%m_%d_%H_%M_%S"`
DIR=${WORKING_DIR}/calibration_${DATESTR}
mkdir -p ${DIR}

CALIB_INPUT=${DIR}/calib_data.json
CALIB_OUTPUT=${DIR}/${TART_CAL_METHOD}_opt_json.json

# Get calibration data
/usr/local/bin/tart_calibration_data --api ${TART_API} --n ${TART_NCAL} --i ${TART_CAL_INT} --file ${CALIB_INPUT}

# Perform optimization
/usr/local/bin/tart_calibrate --api ${TART_API} --file ${CALIB_INPUT} --get-gains --method ${TART_CAL_METHOD} --dir ${DIR}

# Log outputs
CAL_OUTPUT_FILE=${WORKING_DIR}/cal_${DATESTR}.json
mv ${CALIB_OUTPUT} ${CAL_OUTPUT_FILE}
echo "Calibration output is in ${CAL_OUTPUT_FILE}"
/usr/local/bin/tart_upload_gains --api ${TART_API} --gains ${CAL_OUTPUT_FILE} --pw ${TART_LOGIN_PW}

echo "Uploading new antenna positions"
/usr/local/bin/tart_upload_antenna_positions --api ${TART_API} --file ${CAL_OUTPUT_FILE} --pw ${TART_LOGIN_PW}

# Clean up
rm ${DIR}/*
rmdir ${DIR}
