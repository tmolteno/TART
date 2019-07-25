# Getting Started with the TART telescope

This will describe some basic things that can be done with a TART telescope. 

## Imaging from Visibilities

The script 'image_from_vis.py' creates  a synthesis image from visibilities downloaded from the telescope.

Three requests to the telescope get the configuration, calibration gains, and visibility data

    config = api_handler.get_config(api)
    gains = api.get('calibration/gain')
    visibility_data = api.get('imaging/vis')
    
This data is processed using synthesis imaging to produce a crude image.

## Plotting Raw Data

The script 'time_sampled_signals.py' uses a raw data (HDF) file manually downloaded from the telescope (via the web interface), opens the file and prints some samples from antenna 0. 


## Imaging from Raw Data

The script 'image_from_raw.py' create  a very basic image from visibilities downloaded from the telescope.

First download a data file from the telescope web front end (remember to set the telescope to 'raw' mode and check the 'save data' option) to get a recent file. Then execute the following file:

    python3 image_from_raw.py --file  ~/Downloads/data_2019-07-24_22_47_14.353231.hdf

 ![Image generated from raw data][raw_image] 

    
[raw_image]: https://github.com/tmolteno/TART/blob/master/doc/getting_started/raw_image.jpg "TART Raw Data Image"
