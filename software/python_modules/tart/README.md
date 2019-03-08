# TART: Radio-telescope operating software
    
This module is used for the operation and imaging from the open-source Transient Array Radio Telescope (TART).

For more information see the [TART Github repository](https://github.com/tmolteno/TART)

## Authors

* Tim Molteno (tim@elec.ac.nz)
* Max Scheel (max@max.ac.nz)

## Development work
    
If you are developing this package, this should be installed using
```
	make develop
```
in which case changes to the source-code will be immediately available to projects using it.

    
## NEWS

Changelog:

* Version 0.15.0. Python3 changes.
* Version 0.15.1. Major overhaul of tests, more python3 fixes.
* Version 0.15.5. Added EPHEMERIS_SERVER_HOST, POSTGRES_HOST, POSTGRES_USER, POSTGRES_PASSWORD environment variable.
* Version 0.15.6. Added POSTGRES_HOST, POSTGRES_USER, POSTGRES_PASSWORD environment variable.

* Version 1.0.0. Rework database stuff for empirical antennas. New tests.
* Version 1.1b.0 Add hdf5 IO
