"""
A class for GPS satellites.
"""
from tart.imaging import radio_source
from tart.imaging.ephemerides_proxy import EphemeridesProxy
from tart.util import constants
from tart.util import vector
from tart.util import angle

import datetime
import numpy as np


class GpsSatellite(radio_source.RadioSource):
    """This is a class describing a GPS satellite as observed from
    a particular location on Earth."""

    def __init__(self, sv, location, jy=2.5e6):
        radio_source.RadioSource.__init__(self, r=20.0e6, jy=jy)
        self.sv = sv
        self.location = location
        self.ep = EphemeridesProxy.Instance()

    def __repr__(self):
        return "PRN%02i" % self.sv

    def sv_position(self, utc_date):
        x, y, z = self.ep.get_sv_position(utc_date, self.sv)
        # x, y, z = self.ep.get_sv_position_sp3(utc_date, self.sv)
        return x, y, z

    def to_horizontal(self, location, utc_date):
        x, y, z = self.sv_position(utc_date)
        r, el, az = location.ecef_to_horizontal(x, y, z)
        return el, az

    def jansky(self, utc_date):
        x, y, z = self.sv_position(utc_date)
        x0, y0, z0 = self.location.get_ecef()

        r0 = np.array([x0, y0, z0]) - np.array([x, y, z])
        rs = -np.array([x, y, z])

        # Calculate the off nadir angle (angle between rs
        # and r0). This maxes out around 14 degrees
        # An Improved Single Antenna Attitude System Based on GPS Signal Strength
        # C. Wang1, R. A. Walker 2 and M. P. Moody3
        # Cooperative Research Centre for Satellite Systems
        # Queensland University of Technology, Brisbane, QLD, 4000, Australia

        angle_off_nadir = vector.angle_between(r0, rs) - angle.from_dms(
            10.0
        )  # Maximum at 10 degrees

        # Transmit power
        EIRP = 27.0 * angle_off_nadir.cos()  # dbW

        # Now estimate the flux, based on the distance r (in meters)
        r, el, az = self.location.ecef_to_horizontal(x, y, z)
        free_space_loss_db = 10.0 * np.log10(4.0 * np.pi * r ** 2)

        # Received power (over whole band)
        rx_power_dbW = EIRP - free_space_loss_db

        # Received power (per Hz)
        bandwidth = 2.0e6
        rx_power_dbWHz = rx_power_dbW - 10.0 * np.log10(bandwidth)

        # Convert to Jansky
        rx_jansky = 10.0 ** (rx_power_dbWHz / 10.0 + 26.0)
        return rx_jansky

    def radec(self, utc_date):  # Get the RA and Declination
        x, y, z = self.sv_position(utc_date)
        r, el, az = self.location.ecef_to_horizontal(x, y, z)
        ra, dec = self.location.horizontal_to_equatorial(utc_date, el, az)
        return ra, dec

    def velocity(self, utc_date):
        return self.ep.get_sv_velocity(utc_date, self.sv)

    def doppler(self, utc_date):
        dt = datetime.timedelta(seconds=0.5)
        p0 = self.sv_position(utc_date - dt)
        p1 = self.sv_position(utc_date + dt)

        range1 = np.linalg.norm(np.array(p0) - self.location.get_ecef())
        range2 = np.linalg.norm(np.array(p1) - self.location.get_ecef())

        assert isinstance(range1, np.float64), "range1 is not an float: %r" % range1
        assert isinstance(range2, np.float64), "range2 is not an float: %r" % range2

        velocity = range1 - range2
        return velocity / constants.L1_WAVELENGTH

    def get_distance(self, utc_date):
        x, y, z = self.sv_position(utc_date)
        r, el, az = self.location.ecef_to_horizontal(x, y, z)
        return r


"""
GPS BIIA-10 (PRN 32)
1 20959U 90103A   13264.15200711 -.00000016  00000-0  00000+0 0  1080
2 20959  54.3606 219.9575 0116774 342.4080  17.1544  2.00573447167127
GPS BIIA-14 (PRN 26)
1 22014U 92039A   13263.66776611 -.00000029  00000-0  00000+0 0  8942
2 22014  56.0212 280.4726 0208224  71.4390  87.8242  2.00562494148846
GPS BIIA-21 (PRN 09)
1 22700U 93042A   13264.43887392 -.00000083  00000-0  00000+0 0  1442
2 22700  56.3540 334.9730 0166574  99.1584 261.2446  2.00559043148208
GPS BIIA-23 (PRN 04)
1 22877U 93068A   13264.13571874  .00000004  00000-0  00000+0 0  4969
2 22877  53.7393 154.7288 0103827  58.0567 242.5578  2.00570604145844
GPS BIIA-24 (PRN 06)
1 23027U 94016A   13264.46721666 -.00000011  00000-0  00000+0 0  4579
2 23027  53.9882  91.2684 0081894 345.5711  48.9739  2.00562754143163
GPS BIIA-25 (PRN 03)
1 23833U 96019A   13263.94390324 -.00000013  00000-0  10000-3 0  6562
2 23833  53.6122  86.6871 0164764  76.2656 285.6127  2.00557076128134
GPS BIIA-26 (PRN 10)
1 23953U 96041A   13264.13554154 -.00000016  00000-0  00000+0 0  1489
2 23953  54.0332 215.9404 0126745  46.0471 198.8153  2.00564505125924
GPS BIIR-2  (PRN 13)
1 24876U 97035A   13264.43480654 -.00000031  00000-0  00000+0 0  6060
2 24876  56.1678 280.8906 0048464 127.7542 322.9361  2.00561413118467
GPS BIIA-28 (PRN 08)
1 25030U 97067A   13264.33388435 -.00000086  00000-0  00000+0 0  5622
2 25030  57.1741 341.6361 0135135 198.6451  84.5114  2.00562623116358
GPS BIIR-3  (PRN 11)
1 25933U 99055A   13264.05776717  .00000003  00000-0  10000-3 0   411
2 25933  50.9579 137.6735 0143325  69.3165 292.3069  2.00560761102279
GPS BIIR-4  (PRN 20)
1 26360U 00025A   13264.43485363 -.00000014  00000-0  00000+0 0  1331
2 26360  53.1509 211.8036 0053710  72.8182 108.9094  2.00563446 97969
GPS BIIR-5  (PRN 28)
1 26407U 00040A   13263.13207736 -.00000072  00000-0  00000+0 0   841
2 26407  56.4068  38.4423 0190432 258.9763 178.3004  2.00566646 96613
GPS BIIR-6  (PRN 14)
1 26605U 00071A   13264.34772102 -.00000031  00000-0  00000+0 0   994
2 26605  55.7460 279.1707 0076250 245.8023 274.6521  2.00577192 94233
GPS BIIR-7  (PRN 18)
1 26690U 01004A   13264.32975019 -.00000016  00000-0  00000+0 0  9784
2 26690  53.1031 214.8630 0147879 241.2580   4.0622  2.00567355 92659
GPS BIIR-8  (PRN 16)
1 27663U 03005A   13264.33907288 -.00000071  00000-0  00000+0 0  4137
2 27663  56.4951  38.0454 0070396   5.9085 341.2954  2.00558878 78008
GPS BIIR-9  (PRN 21)
1 27704U 03010A   13264.26463368  .00000003  00000-0  00000+0 0  2948
2 27704  53.3862 154.8888 0207292 239.6820  15.6940  2.00568743 76804
GPS BIIR-10 (PRN 22)
1 28129U 03058A   13264.19180442 -.00000017  00000-0  00000+0 0  8653
2 28129  52.9746 215.0049 0069796 244.0199 230.9104  2.00566216 71504
GPS BIIR-11 (PRN 19)
1 28190U 04009A   13262.91358984  .00000016  00000-0  00000+0 0  8641
2 28190  55.2105 100.3732 0094858  20.5097 288.7818  2.00555497 69658
GPS BIIR-12 (PRN 23)
1 28361U 04023A   13264.35447721 -.00000031  00000-0  00000+0 0  6797
2 28361  54.6911 275.6843 0089585 199.5709 221.6402  2.00566099 67768
GPS BIIR-13 (PRN 02)
1 28474U 04045A   13264.12729704  .00000004  00000-0  00000+0 0  5669
2 28474  53.7946 153.7946 0127152 214.1585  48.0255  2.00556354 65128
GPS BIIRM-1 (PRN 17)
1 28874U 05038A   13263.12125517  .00000011  00000-0  00000+0 0  8253
2 28874  55.3715  97.4088 0087119 235.1317 124.5596  2.00562310 58511
GPS BIIRM-2 (PRN 31)
1 29486U 06042A   13264.26023969 -.00000084  00000-0  00000+0 0  2292
2 29486  56.1466 336.5502 0081195 316.5190  69.5394  2.00563866 51246
GPS BIIRM-3 (PRN 12)
1 29601U 06052A   13264.21866396 -.00000073  00000-0  00000+0 0   483
2 29601  56.4572  36.9938 0044537  16.3007  19.9535  2.00554418 50159
GPS BIIRM-4 (PRN 15)
1 32260U 07047A   13261.11505200 -.00000013  00000-0  00000+0 0  9611
2 32260  53.8345 273.3594 0055427   6.4973  76.2259  2.00576209 43478
GPS BIIRM-5 (PRN 29)
1 32384U 07062A   13264.19489889  .00000001  00000-0  00000+0 0  9920
2 32384  55.4105  97.8740 0016445 298.9424 347.4493  2.00575463 42265
GPS BIIRM-6 (PRN 07)
1 32711U 08012A   13264.39826917 -.00000085  00000-0  10000-3 0  7509
2 32711  55.8618 336.2554 0069495 196.7741 163.0538  2.00564324 40483
GPS BIIRM-7 (PRN 30)
1 34661U 09014A   13264.31903903 -.00000075  00000-0  00000+0 0  7493
2 34661  56.0906  35.7052 0061733  49.2470 323.4618  2.00561535 32993
GPS BIIRM-8 (PRN 05)
1 35752U 09043A   13264.20007641 -.00000015  00000-0  00000+0 0  5132
2 35752  54.3301 214.8640 0032418  12.0141 235.0576  2.00559013 30042
GPS BIIF-1  (PRN 25)
1 36585U 10022A   13264.06342811 -.00000077  00000-0  00000+0 0  3238
2 36585  55.8153  34.7321 0025923  37.9641 213.2629  2.00558901 24299
GPS BIIF-2  (PRN 01)
1 37753U 11036A   13264.26241510  .00000006  00000-0  00000+0 0  7840
2 37753  55.0259 155.1748 0023049  16.6640 104.2417  2.00561700 15988
GPS BIIF-3  (PRN 24)
1 38833U 12053A   13264.08040087 -.00000085  00000-0  00000+0 0  3312
2 38833  54.9148 334.5224 0010680 356.2319   3.8200  2.00565997  7042
GPS BIIF-4  (PRN 27)
1 39166U 13023A   13264.43514503 -.00000004  00000-0  10000-3 0  1097
2 39166  55.0631  94.8384 0002691  25.5924 334.4770  2.00564685  2576
"""
