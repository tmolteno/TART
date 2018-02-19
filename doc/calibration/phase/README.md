# Gain/Phase Calibration of the TART telescope

Script tart_calibrate.sh should be added as a cron job on a reasonably powerful machine, and run every two hours.

    # m h  dom mon dow   command
    15 */2 * * * cd /home/tim/github/TART/doc/calibration/phase/; sh tart_calibrate.sh
