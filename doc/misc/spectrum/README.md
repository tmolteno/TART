# Generating a Frequency Spectrum from RAW data

This note shows how to generate frequency spectra for the TART telescope from raw data files.

## Enabling raw data

Log into the web interface, and select the 'Raw Data' tab. Make sure the 'Save data' checkbox is ticked. The Number of samples exponent is the log_2 of the number of raw data samples that will be stored. The Integration time corresponding to this number of samples is shown.

log out of the web interface, and in the main screen a list of raw data files will be shown. These can be downloaded from the browser, or an automated script can be used to download all files. See the  [API documentation](https://tart.elec.ac.nz/doc/#api-Acquisiton-get_raw_data_file_handles) on the /raw/data API request for how to do this.


## Usage:

This assumes that the data has been downloaded from the browser into the directory:
    ~/Downloads/data_2019-07-24_22_47_14.353231.hdf
Executing the following command will plot the spectrum for each antenna.
    python3 raw_data_info.py --file ~/Downloads/data_2019-07-24_22_47_14.353231.hdf --show --spectrum

    
