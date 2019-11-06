# Create a Measurement Set from a TART JSON file

Install requires casacore and dask-ms

    sudo aptitude install python3-casacore
    sudo pip3 install dask-ms

Then run using

    python3 json2ms.py --ms ${MS} --json data_2019_08_04_21_38_31_UTC.json

## TODO:

This is a cludge based on the dask-ms code. THe correct mapping for the polarizations and whatever else CASA requires is still to be done.
