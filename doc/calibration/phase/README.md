# Gain/Phase Calibration of the TART telescope

Author: Tim Molteno tim@elec.ac.nz (c) 2017-2018

Once the positions of the antennas are known, then phase calibration is possible from observing the measured phases from a known source position.

**NOTE: Every time the telescope is powered off, the phase calibration will need to be redone**


## Automatic Calibration from Satellite Positions

A software container that will automatically do this (and install the required pre-requisites) is described in [software/containers/calibration_server](../../../software/containers/calibration_server/README.md)


## Manual Single-Point Source Calibration

The remainder of this README will describe how to do manual calibration. 

* Use a single tile with 6 receivers. These are numbered 0...5. 
* The antenna positions have been measured and recorded as described [here](../positions/README.md)
* A noise source is placed at a known elevation and azimuth.

Under these conditions, follow the procedure described in [POINT_SOURCE.md](./POINT_SOURCE.md)
