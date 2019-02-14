## Position calibrations

Author: Tim Molteno tim@elec.ac.nz 2017.

This notebook is used to calibrate the antenna positions. It generates a JSON output file that
can be used to upload the calibrated positions via the telescope web API

First install the tart python modules

    sudo pip install tart tart-tools
    
Then do the upload

    tart_upload_antenna_positions --api ${TART_API} --file ${CAL_OUTPUT_FILE} --pw ${TART_LOGIN_PW}

### Making modifications permanent

The positions can also be updated by modifying the json file [software/telescope_web_api/config_data/calibrated_antenna_positions.json](../../../software/containers/telescope_web_api/config_data/calibrated_antenna_positions.json). These positions are then permanently modified.


**NOTE: Positions updated via the web API are not permanently modified. On software restart they will revert to those in the file above**
