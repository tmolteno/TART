## Radio-Hub Calibration

This calibration requires a working telescope core. There are several steps required to verify that each radio is working correctly.

Required Equipment:
- RF signal generator (ideally) or a noise source.
- Active antenna bybass board (alternatively remove the inductor that powers the active antenna on each radio.
- SMA splitter connector

### Radio Reception Tests

Use the RF signal generator to create a signal with a strength of -110 dBW. Connect this signal to the antenna input. Take care to bypass the active antenna power.

Check 'Capture Data' in the web interface.

Download a captured data file. Verify that the spectrum of the signal matches the expected signal applied.  Correlate the downloaded data with a sequence (if your RF generator is capable of generating a PRN sequence). 

Plot the Correlation Amplitude as a function of signal strength between -100 and -150 dBW.

### Visibility Tests 

Place the telescope in 'vis' mode via the web interface.  Use the SMA splitter cable. Connect the RF signal at -110dBW to two antenna inputs. Measure the visibility, and plot this as a function of signal strength between -100 and -150 dBW.

Check that the visibility phase is close to zero. Any non-zero phase indicates that the signal from one of the antennas is delayed. Low visibility amplitude indicates that one of the radios is not working well. This should have been found in the Reception tests earlier.

 
