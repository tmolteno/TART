## Guide to Calibration

Author: Tim Molteno tim@elec.ac.nz (c) 2018.

The telescope will not operate without calibration. The calibration will determine the positions of the annteas, as well as the phases of the radio receivers. Once calibrated, the calibration values are available through through the telescope web interface so that images can be made from the data. 

Calibration of the telescope requires two steps. The first is to calibrate the positions of the antennas. This is done only once. The second step is to calculate the radio-receiver gains and phase offsets. This is normally done at regular intervales (every few hours or possibly few days).


### Antenna Positions

A procedure and some software for estimating the antenna positions is described in the positions directory. Once these positions are known, they are entered into a telescope calibration file located [software/telescope_web_api/config_data/calibrated_antenna_positions.json](../../software/containers/telescope_web_api/config_data/calibrated_antenna_positions.json)
    
An example of a calibration file is shown below:

    [[-0.16674626294077627, 0.83926973244320302, 0], [0.1528167058971408, 0.81138588501325737, 0], [-0.29700909228308842, 0.70509970862366411, 0], [0.57166882217141801, 1.248973481606853, 0], [-0.13757589499456357, 1.4147200688180135, 0], [0.32087802506335883, 1.3731869111783139, 0], [1.0165521355681872, -0.46882273374033218, 0], [1.1846054140376161, -0.19466384623953165, 0], [1.0801225516077622, -0.65158980463811544, 0], [0.99613714285515575, 0.37536221523265623, 0], [0.5209481861041273, -0.16784810810616724, 0], [0.7763526713548774, 0.21674069687104397, 0], [-0.46213370078932842, -1.0106143886739192, 0], [-0.19670086627170769, -1.1762477499268427, 0], [-0.64174044964781285, -1.0689874626420195, 0], [0.377973581041327, -0.97886923132499948, 0], [-0.18511601963632354, -0.51833682029545647, 0], [0.20654108839824029, -0.75610500514364432, 0], [-0.84278598903931101, 0.40241244597928449, 0], [-1.1125141153781231, 0.25626615672856945, 0], [-0.80143781237664125, 0.58622302316860908, 0], [-1.2141115271544058, -0.32786308061566805, 0], [-0.55621794637209787, -0.089501933686407326, 0], [-0.95578610701110178, -0.29349773022046144, 0]]

This is a list of the coordinates in the East-North plane. 

### Gains and Phases

There are two methods to calculate the gains and phases. The easiest can be used if the telescope is looking upwards. In this case the gains and phases can be calculated automatically from known satellite positions. Software for doing this automatically is described in [phase/README.md](phase/README.md).

**NOTE: Every time the telescope is powered off, the phase calibration will need to be redone**

Because of this, the phase calibration is typically done at regular intervals as a cron job.'

