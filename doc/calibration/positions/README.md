## Position calibrations

Author: Tim Molteno tim@elec.ac.nz 2017.

This notebook is used to calibrate the antenna positions. It generates a JSON output file that
can be used to upload the calibrated positions via the telescope web API

    python upload_antenna_positions --file xxx.json --host https://tart.elec.ac.nz/signal --pw <insert password here>

THe positions can also be updated by modifying the json file [software/telescope_web_api/config_data/calibrated_antenna_positions.json](../../../software/containers/telescope_web_api/config_data/calibrated_antenna_positions.json). These positions are then permanently modified.
