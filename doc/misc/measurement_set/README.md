# Create a Measurement Set from a TART JSON file

Install requires casacore and some tart tools tart, tart-tools and tart2ms

    sudo aptitude install python3-casacore
    sudo pip3 install tart2ms tart tart-tools wsclean


## Getting a measurement set

    tart_calibration_data --file tart_data.json --n 1
    tart2ms --json tart_data.json --ms tart_data.ms

## Imaging Using WsClean

    export OPENBLAS_NUM_THREADS=1; wsclean -v -scale 0.1 -size 1700 1700  \
                -make-psf -pol RR  -use-wgridder \
                -auto-threshold 3 -mgain 0.05 -gain 0.05 -niter 2000 tart_data.ms
