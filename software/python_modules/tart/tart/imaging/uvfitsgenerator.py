"""
Generate UVFITS file from visibility object.
"""

from astropy.io import fits
import numpy as np

from tart.imaging import tart_util
from tart.imaging import radio_source
from tart.imaging import location

from tart.simulation import antennas

from tart.util import angle
from tart.util import skyloc
from tart.util import constants


def encode_baseline(a1, a2):
    """Encode the baseline. Use the miriad convention to handle more than 255 antennas (up to 2048)."""
    if a1 > a2:
        te = a2
        a2 = a1
        a1 = te

    if a2 > 255:
        return (
            a1 * 2048 + a2 + 65536
        )  # this is backwards compatible with the standard UVFITS convention
    else:
        return a1 * 256 + a2


def decode_baseline(bl):
    """Decode a baseline using same (extended) miriad format as above"""
    if bl > 65536:
        bl -= 65536
        a2 = bl % 2048
        a1 = (bl - a2) / 2048
        return a1, a2
    else:
        a2 = bl % 256
        a1 = (bl - a2) / 256
        return a1, a2


class UVFitsGenerator(object):
    """ Generate a UVFITS file for a target source from an array of visibilities."""

    def __init__(self, cal_vis_list, phase_center):  # FIXME
        # self.phase_center_elevation = phase_center_elevation
        # self.phase_center_azimuth = phase_center_azimuth
        vt = cal_vis_list[0]

        c = vt.get_config()
        ant_p = np.asarray(c.get_antenna_positions())

        self.config = c
        self.v_array = cal_vis_list
        self.n_baselines = len(vt.get_baselines())
        self.loc = c.get_loc()  # Assume the array isn't moving

        if phase_center is None:
            ra, dec = location.get_loc(vt.get_config()).horizontal_to_equatorial(
                vt.get_timestamp(), angle.from_dms(90), angle.from_dms(0)
            )
            self.phase_center = radio_source.CosmicSource(ra, dec, 1e10)
        else:
            self.phase_center = phase_center

    def gen_vis_table(self):
        """Generate visibility table and fill payload."""

        baselines = []
        ant_pos = self.config.get_antenna_positions()
        loc = self.config.get_loc()
        for cal_vis in self.v_array:
            ra, dec = self.phase_center.radec(cal_vis.vis.timestamp)
            # v.rotate(skyloc.Skyloc(ra, dec))
            uu_a, vv_a, ww_a = cal_vis.get_all_uvw()
            bls = cal_vis.get_baselines()
            datestamp = tart_util.get_julian_date(cal_vis.vis.timestamp) - int(
                tart_util.get_julian_date(cal_vis.vis.timestamp) + 0.5
            )
            for uu, vv, ww, b in zip(uu_a, vv_a, ww_a, bls):
                baseline = {}
                [i, j] = b
                # print((np.array(a1.enu) - np.array(a0.enu)), uu, vv, ww)
                # arcane units of UVFITS require u,v,w in nanoseconds
                baseline["UU"] = uu * constants.L1_WAVELENGTH / constants.V_LIGHT
                baseline["VV"] = vv * constants.L1_WAVELENGTH / constants.V_LIGHT
                baseline["WW"] = ww * constants.L1_WAVELENGTH / constants.V_LIGHT
                baseline["BASELINE"] = encode_baseline(i + 1, j + 1)
                baseline["DATE"] = datestamp
                # DATE FIXME ?
                baselines.append(baseline)

        freqs = np.array([1545.0])
        pols = np.array(["+"])

        data = np.zeros(
            (len(self.v_array) * self.n_baselines, 1, 1, len(freqs), len(pols), 3)
        )

        for i, v in enumerate(self.v_array):
            for k, b in enumerate(cal_vis.get_baselines()):
                for l, _ in enumerate(freqs):
                    for j, _ in enumerate(pols):
                        vis = v.get_visibility(b[0], b[1])
                        re = vis.real
                        img = vis.imag
                        w = np.ones(1)
                        data[i * self.n_baselines + k, 0, 0, l, j, :] = [re, img, w]

        hdu = fits.GroupsHDU(
            fits.GroupData(
                data,
                parnames=["UU", "VV", "WW", "BASELINE", "DATE"],
                bitpix=-32,
                pardata=[
                    [b["UU"] for b in baselines],
                    [b["VV"] for b in baselines],
                    [b["WW"] for b in baselines],
                    [b["BASELINE"] for b in baselines],
                    [b["DATE"] for b in baselines],
                ],
            )
        )
        return hdu

    def gen_ant_table(self):
        """Generate antenna table."""
        v = self.v_array[0]
        n_ant = self.config.get_num_antenna()
        ant_pos = self.config.get_antenna_positions()
        v.antennas = []
        for i in range(n_ant):
            antenna = {}
            antenna["ANNAME"] = "ANT%i" % (i + 1)  # there is no antenna 0
            antenna["STABXYZ"] = ant_pos[i]
            antenna[
                "ORBPARM"
            ] = 0.0  # SET TO 0. - not used, because 'NUMORB' in header is    0
            antenna["NOSTA"] = i + 1  # there is no antenna 0
            antenna["MNTSTA"] = 0
            antenna["STAXOF"] = 0.0
            antenna["POLTYA"] = "X"
            antenna["POLAA"] = [0.0]
            antenna["POLCALA"] = np.array([0.0, 0.0, 0.0])
            antenna["POLTYB"] = "Y"
            antenna["POLAB"] = [0.0]
            antenna["POLCALB"] = np.array([0.0, 0.0, 0.0])
            v.antennas.append(antenna)

        c1 = fits.Column(
            name="ANNAME", format="8A", array=[a["ANNAME"] for a in v.antennas]
        )
        c2 = fits.Column(
            name="STABXYZ",
            format="3D",
            array=[a["STABXYZ"] for a in v.antennas],
            unit="METERS",
        )
        c3 = fits.Column(
            name="NOSTA", format="1J", array=[a["NOSTA"] for a in v.antennas]
        )
        c4 = fits.Column(
            name="MNTSTA", format="1J", array=[a["MNTSTA"] for a in v.antennas]
        )
        c5 = fits.Column(
            name="STAXOF",
            format="1E",
            array=[a["STAXOF"] for a in v.antennas],
            unit="METERS",
        )
        c6 = fits.Column(
            name="POLTYA", format="1A", array=[a["POLTYA"] for a in v.antennas]
        )
        c7 = fits.Column(
            name="POLAA",
            format="1E",
            array=[a["POLAA"] for a in v.antennas],
            unit="DEGREES",
        )
        c8 = fits.Column(
            name="POLCALA", format="3E", array=[a["POLCALA"] for a in v.antennas]
        )
        c9 = fits.Column(
            name="POLTYB", format="1A", array=[a["POLTYB"] for a in v.antennas]
        )
        c10 = fits.Column(
            name="POLAB",
            format="1E",
            array=[a["POLAB"] for a in v.antennas],
            unit="DEGREES",
        )
        c11 = fits.Column(
            name="POLCALB", format="3E", array=[a["POLCALB"] for a in v.antennas]
        )
        c0 = fits.Column(
            name="ORBPARM", format="1D", array=[a["ORBPARM"] for a in v.antennas]
        )

        return fits.BinTableHDU.from_columns(
            [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c0]
        )

    def update_vis_header(self):
        """Update visibility header."""

        v = self.v_array[0]
        params = ["UU", "VV", "WW", "BASELINE", "DATE"]
        ctypes = ["STOKES", "FREQ", "RA", "DEC"]

        self.vis_header.set("OBJECT", "Full Sky")

        # without rotation of the visibilities is has to be this:
        # ra, dec = self.loc.horizontal_to_equatorial(v.vis.timestamp, angle.from_dms(90.), angle.from_dms(0.))
        # with rotation is has to be:

        ra, dec = self.phase_center.radec(v.vis.timestamp)

        self.vis_header.set("OBSRA", ra.to_degrees())  # Antenna pointing center
        self.vis_header.set("OBSDEC", dec.to_degrees())  # Antenna pointing center

        self.vis_header.set("TELESCOP", "TART")
        self.vis_header.set("INSTRUME", "TART")
        self.vis_header.set("EPOCH", 2000.0)
        self.vis_header.set("BSCALE", 1)
        self.vis_header.set(
            "DATE-OBS",
            "%i-%02i-%02iT00:00:00.0"
            % (v.vis.timestamp.year, v.vis.timestamp.month, v.vis.timestamp.day),
        )

        for i, par in enumerate(params):
            self.vis_header.set("PTYPE%i" % (i + 1), par)
            self.vis_header.set("PSCAL%i" % (i + 1), 1.0)
            self.vis_header.set("PZERO%i" % (i + 1), 0.0)

        self.vis_header.set(
            "PZERO5", int(tart_util.get_julian_date(v.vis.timestamp) + 0.5)
        )

        self.vis_header.set("CRVAL2", 1.0)
        self.vis_header.set("CRPIX2", 1.0)
        self.vis_header.set("CDELT2", 1.0)
        self.vis_header.set("CTYPE2", "COMPLEX")

        for i, ctyp in enumerate(ctypes):
            self.vis_header.set("CTYPE%d" % (i + 3), ctyp)
            self.vis_header.set("CRVAL%d" % (i + 3), 1.0)
            self.vis_header.set("CRPIX%d" % (i + 3), 1.0)
            self.vis_header.set("CDELT%d" % (i + 3), 1.0)

        # set the input pol type.
        # for circular CRVAL3 should be -1, CDELT3 -1
        # for linear     CRVAL3 should be -5, CDELT3 -1
        # for Stokes     CRVAL3 should be    1, CDELT3    1
        # header['CRVAL3']    = -5.
        # header['CDELT3']    = -1.

        pol_typ = 1.0  # FIXME
        # pol_typ =    Obs.pol_typ
        self.vis_header.set("CRPIX3", pol_typ)
        if pol_typ < 0:
            self.vis_header.set("CDELT3", -1.0)
        else:
            self.vis_header.set("CDELT3", 1.0)

        self.vis_header.set("CRPIX4", 1.0)  # something .... n_freq/2 +1
        self.vis_header.set("CDELT4", v.vis.config.get_bandwidth())  # freq delta
        self.vis_header.set(
            "CRVAL4", v.vis.config.get_operating_frequency()
        )  # center freq

        ra, dec = self.phase_center.radec(v.vis.timestamp)

        self.vis_header.set("CRVAL5", ra.to_degrees())  # SOURCE RA    PARAMETER
        self.vis_header.set("CRVAL6", dec.to_degrees())  # SOURCE DEC PARAMETER

        self.vis_header.add_history(
            "AIPS WTSCAL =    1.0"
        )  # has to be in here for some reason
        self.vis_header.add_comment(
            "written by the TART UV FITS writer. ELEC Otago Uni"
        )

    def update_ant_header(self):
        """Update antenna header."""

        v = self.v_array[0]

        self.ant_header.set("TFORM3", "1J")  #
        self.ant_header.set("TFORM4", "1J")  #
        self.ant_header.set(
            "TFORM5", "1E"
        )  #     replace (J,E,D) with (1J,1E,1D) to match difmap requirements
        self.ant_header.set(
            "TFORM7", "1E"
        )  # # ra and dec of sun here! FIXME - move to fixed phase center in ra/dec
        self.ant_header.set("TFORM10", "1E")  #
        self.ant_header.set("TFORM12", "1D")  #

        self.ant_header.set("EXTNAME", "AIPS AN")
        self.ant_header.set("XTENSION", "BINTABLE")
        arr_loc = self.loc.get_ecef()
        self.ant_header.set("ARRAYX", arr_loc[0])  #
        self.ant_header.set(
            "ARRAYY", arr_loc[1]
        )  #    TART POSITION in ECEF coordinates
        self.ant_header.set("ARRAYZ", arr_loc[2])  #
        self.ant_header.set("FREQ", v.vis.config.get_operating_frequency())
        # /* GSTIAO is the GST at zero hours in the time system of TIMSYS (i.e. UTC) */
        # mjd = trunc(data->date[0] - 2400000.5);
        # temp = slaGmst(mjd)*180.0/M_PI;    # technically, slaGmst takes UT1, but it won't matter here.
        # fits_update_key(fptr,TDOUBLE,"GSTIA0",&temp , NULL, &status);
        self.ant_header.set("GSTIA0", self.loc.GST(v.vis.timestamp).to_degrees())
        self.ant_header.set("DEGPDY", 3.60985642988e02)  # Rotation per day

        self.ant_header.set(
            "RDATE",
            "%d-%02d-%02dT00:00:00.0"
            % (v.vis.timestamp.year, v.vis.timestamp.month, v.vis.timestamp.day),
        )
        self.ant_header.set("POLARX", 0.0)
        self.ant_header.set("POLARY", 0.0)
        self.ant_header.set("UT1UTC", 0.0)
        self.ant_header.set("DATUTC", 0.0)
        self.ant_header.set("TIMSYS", "UTC")
        self.ant_header.set("ARRNAM", "TART")
        self.ant_header.set("NUMORB", 0)
        self.ant_header.set("NOPCAL", 3)
        self.ant_header.set("FREQID", -1)
        self.ant_header.set("IATUTC", 33.0)
        self.ant_header.set("EXTVER", 1)
        # self.ant_header.set('XYZHAND','RIGHT')

    def write(self, filename):
        """Generate tables and headers and write UVFITS file."""
        vis_table = self.gen_vis_table()
        ant_table = self.gen_ant_table()

        self.vis_header = vis_table.header
        self.update_vis_header()
        self.ant_header = ant_table.header
        self.update_ant_header()

        hdulist = fits.hdu.hdulist.HDUList([vis_table, ant_table])
        hdulist.writeto(filename)
