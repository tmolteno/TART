## Using a single point source for TART calibration

Hopefully this will describe how to use the calibration routines in tart_tools to do a custom calibration from a single point source
Assume the elevation and azimuth of the point source are el, az.

### Pre-requisites

This HOWTO uses the tart_tools package - this is a handy set of routines for getting data from the telescope.

    sudo pip3 install tart_tools
    
You will need to know the URL of your telescope api. This is stored in the variable TART_API. The public tart telescope uses 
In a terminal window type

    export TART_API=https://tart.elec.ac.nz/signal
    
### Step 1. 


The first step is to put the source in the known location, and get the calibration data. There is a program (tart_calibration_data) in tart_tools that does this.

Switch on your source, and execute the following command.

    /usr/local/bin/tart_calibration_data --api ${TART_API} --n 1 --i 0 --file calib_data.json

This will create a file in the current directory called calib_data.json that contains all the visibilities and also some satellite positions.
It should look like:

    {
        "info": {
            "info": {
                "L0_frequency": 1571328000.0,
                "bandwidth": 2500000.0,
                "baseband_frequency": 4092000.0,
                "location": {
                    "alt": 270.0,
                    "lat": -45.85177,
                    "lon": 170.5456
                },
                "name": "Signal Hill - Dunedin",
                "num_antenna": 24,
                "operating_frequency": 1575420000.0,
                "sampling_frequency": 16368000.0visibility data
            }
        },
        "ant_pos": [
            [
                -0.2197974550032495,
                0.8269625618938828,
                0.0
            ],
            <.. Lots more antenna positions here>
            [
                -0.9351798355597619,
                -0.353704219882117,
                0.0
            ]
        ],
        "gains": {
            "gain": [
                1.0,
                1.9535,
                1.7564,
                1.5493,
                1.854,
                1.2414,
                2.2351,
                1.6668,
                1.9121,
                2.2245,
                1.9336,
                1.4494,
                1.1027,
                1.8433,
                1.8277,
                1.4885,
                1.8811,
                2.0096,
                1.3902,
                1.9146,
                1.3583,
                1.287,
                2.1017,
                2.3469
            ],
            "phase_offset": [
                0.0,
                -1.1614,
                -0.7987,
                -2.9966,
                -0.5505,
                2.3404,
                0.6512,
                2.4634,
                -2.9613,
                1.0048,
                2.3228,
                -1.0798,
                2.2187,
                2.0924,
                -1.7277,
                0.137,
                -2.8215,
                3.0439,
                2.9209,
                0.6882,
                -1.9804,
                -0.3136,
                -0.5127,
                -2.2903
            ]
        },
        "data": [
            [
                {
                    "data": [
                        {
                            "i": 0,
                            "im": -0.003594674260098835,
                            "j": 1,
                            "re": 0.05208605321947359
                        },
                        {
                            "i": 0,
                            "im": 0.05036310473463818,
                            "j": 2,
                            "re": 0.0010756546919338862
                        },

                        <.. Lots more visibility data here>
                        
                        {
                            "i": 22,
                            "im": -0.016830602167805925,
                            "j": 23,
                            "re": 0.045235982558923686
                        }
                    ],
                    "timestamp": "2019-05-10T00:39:27.377Z"
                },
                [
                    {
                        "az": 64.58546,
                        "el": 14.216405,
                        "jy": 1500000.0,
                        "name": "CRW (WAAS/PRN 135)",
                        "r": 40134476.9
                    },
                    
                    <.. Lots more source position here>
                    
                    {
                        "az": 358.618284,
                        "el": 26.642567,
                        "jy": 10000.0,
                        "name": "sun",
                        "r": 10000000000.0
                    }
                ]
            ]
        ]
    }


### Step 2: Modify the data file.

The file calib_data.json contains all the data necessary for the calibration routine. We need to remove the source locations for the satellites, since there is only one source. Edit this file, the sources are at the end. Note that each known source looks like

    {
        "az": 358.618284,
        "el": 26.642567,
        "jy": 10000.0,
        "name": "sun",
        "r": 10000000000.0
    }

Remove all but one of the sources, and replace it with suitable values (the known elevation and azimuth of your calibration source (in degrees), and the distance $r$ to the source).

                [
                    {
                        "az": 0.0,
                        "el": 45.0,
                        "jy": 100000.0,
                        "name": "my source",
                        "r": 25.0
                    }
                ]

Leave the rest of the file unmodified, and save it.

### Step 3: Run the calibration using the modified file.

    # Perform optimization
    /usr/local/bin/tart_calibrate --api ${TART_API} --file ./calib_data.json --method BH --dir .

This will take a long time (a couple of hours). During the process, images of the current state of the optimization should fill the current directory. These are called 

    opt_slice_NNNNN.png
    BH_XXXXX_opt_full_NNNNN.png

Hopefully these files will start to look like a single point source!

The final calibration output will be in the file BH_opt_json.json


### Step 4: Uploading the new antenna gains.

Now you can upload the new gains to the telescope.

This requires the password to your telescope API, stored in the variable TART_LOGIN_PW

    /usr/local/bin/tart_upload_gains --api ${TART_API} --gains BH_opt_json.json --pw ${TART_LOGIN_PW}

