# TART Hardware Description

The TART telescope consists of three major components.

* Radio Front End Module
* Radio Hub Module
* Base Station

## Radio Front End


The radio front end is a low-noise radio that digitizes the incoming signal at a sample rate of 16.368 MHz. An external clock input is used to synchronize the sampling. A full tart telescope consists of 24 radio front end modules arranged into four 'Tiles'

-> ![TART GPS L1 radio front end][radio_front_end_image] <-


## Radio Hub Module

A radio hub module controls six radio front ends. It is responsible for generating low-noise power, 
and removing jitter from the main system clock (generated from the base station).
The radio hub distributes this clean clock to each of the radio front end modules. The data from each radio is converted to RS422 differential signaling and fed back down Cat-6 UTP cabling back to the basestation. 


-> ![Radio Hub Module][radio_hub_image] <-

This image shows a radio hub with six radio front end modules mounted.

## Base Station


-> ![Radio Telescope Basestation][basestation_image] <-


[radio_front_end_image]: https://github.com/tmolteno/TART/blob/master/doc/img/front_end.jpg "TART Radio Module"
[radio_hub_image]: https://github.com/tmolteno/TART/blob/master/doc/img/radio_hub_photo.jpg "TART Radio Hub"
[basestation_image]: https://github.com/tmolteno/TART/blob/master/doc/img/control_board_photo.jpg "TART Basestation"
