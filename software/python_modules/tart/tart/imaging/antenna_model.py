"""
    Antenna Models for TART
    
    Author: Tim Molteno 2014-2019 tim@elec.ac.nz
    Copyright GPLv3
"""
import numpy as np
import healpy as hp

from tart.imaging import location
from tart.util import angle
from tart.util.db import db_connect, db_remove_antenna_measurements, db_insert_row


class AntennaModel(object):
    """Base class for all Antenna models."""

    def get_gain(self, el, az):
        """Dummy method. All base classes should override this"""
        raise RuntimeError("You should be using a subclass of AntennaModel")

    def get_gain_equatorial(self, loc, utc_time, ra, decl):
        """Return the gain from equatorial co-ordinates"""
        el, az = loc.equatorial_to_horizontal(utc_time, ra, decl)
        return self.get_gain(el, az)


class IdealHemisphericalAntenna(AntennaModel):
    """Ideal Hemispherical Antenna."""

    """Return the gain """

    def get_gain(self, el, az):
        if el.to_degrees() < 0.0:
            return 0.0
        if el.to_degrees() > 90:
            return 0.0
        return 1.0


class GpsPatchAntenna(AntennaModel):
    """A more realistic model of a GPS patch antenna."""

    def get_gain(self, el, az):
        if el.to_degrees() < 5.0:
            return 0.0
        if el.to_degrees() > 90:
            return 0.0
        return 1.0


def hp_interpolator(map_, el, az, n_pix=4):
    NSide = hp.pixelfunc.get_nside(map_)
    direction = np.array([np.pi / 2.0 - el.to_rad(), az.to_rad()])
    steplength = hp.pixelfunc.max_pixrad(NSide)
    for i, r in enumerate(np.arange(steplength, np.pi, steplength)):
        pixels = np.array(
            hp.query_disc(NSide, hp.ang2vec(direction[0], direction[1]), r)
        )
        filled = np.where(map_[pixels] > -1.0)[0]
        l = len(filled)
        if l >= n_pix:
            # print(i, l)
            filled_pixel = pixels[filled]
            filled_pixel_directions = hp.pix2vec(NSide, filled_pixel)
            angular_distance = hp.rotator.angdist(direction, filled_pixel_directions)
            if angular_distance.min() == 0.0:  # do we really want this?
                return map_[filled_pixel[angular_distance.argmin()]]
            return np.average(
                map_[filled_pixel], weights=np.power(1.0 / angular_distance, 2)
            )


# def hp_spheric_harmonics(hp_alm, el, az, mmax=-1):
# from scipy.special import sph_harm
# if mmax==-1:
# lmax = hp.Alm.getlmax(len(hp_alm))
# mmax = lmax
# else:
# lmax = hp.Alm.getlmax(len(hp_alm),mmax=mmax)

# print(lmax, mmax)
# out = 0.
# azimutal_angle = az.to_rad()
# polar_angle = el.to_rad()-np.pi/2.
# for l in range(lmax+1):
# out += np.real(hp_alm[hp.Alm.getidx(lmax,l,0)] * sph_harm(0, l, azimutal_angle, polar_angle))
# for m in range(1, 1+min(l , mmax)):
# Ylm = sph_harm(m, l, azimutal_angle, polar_angle)
# out += np.real(hp_alm[hp.Alm.getidx(lmax,l,m)] * Ylm)
# out += np.real(hp_alm[hp.Alm.getidx(lmax,l,m)] * Ylm.conj()/np.power(-1.,m))
# return out


def gen_interpolation_map(points, values, antenna_num, nside_exp):
    nside = np.power(2, nside_exp)
    npix = hp.nside2npix(nside)
    hp_map_avg = np.ones(npix) * hp.UNSEEN
    theta = np.array([90.0 - i[0] for i in points]) * np.pi / 180.0
    phi = np.array([i[1] for i in points]) * np.pi / 180.0

    pix = hp.pixelfunc.ang2pix(nside, theta, phi)

    values = np.array(values)

    dic = {}
    for i, p in enumerate(pix):
        if dic.get(p) is None:
            dic[p] = []
        dic[p].append(values[i])

    pixel_dict = {}
    for p in dic:
        pixel_dict[p] = np.array(dic[p]).mean()
        hp_map_avg[p] = pixel_dict[p]

    map_mean = np.array([pixel_dict[key] for key in pixel_dict]).mean()

    for p in dic:
        # pixel_dict[p] = (pixel_dict[p]-map_mean)*(1.*len(hp_map_avg))/(4.*np.pi)
        hp_map_avg[p] = pixel_dict[p]

    return (hp_map_avg, pixel_dict)


class EmpiricalAntenna(AntennaModel):
    """An antenna model that has a radiation pattern built up
    from measured GPS signal correlation amplitudes. This
    incorporates an horizon model as well.

    The model can be serialized into JSON form using the to_json()
    method. And reconstructed from JSON using the from_json()
    """

    def __init__(self, antenna_num):
        self.sv = []  # A list of sv numbers - essentially an identifier
        self.distances = []  # A list of sv distances - essentially an identifier
        self.points = []  # The data is stored as a list of el, az.
        self.values = []  # The data is stored as a list of amplitudes.
        self.recalculate = True
        self.antenna_num = antenna_num
        self.interpolation_cache = []

    def add_measurement(self, el, az, amplitude, sv=-1, distance=-1):
        self.points.append([el.to_degrees(), az.to_degrees()])
        self.values.append(amplitude)
        self.sv.append(sv)
        self.distances.append(distance)
        self.recalculate = True

    def get_gain(
        self,
        el,
        az,
        n_pix=4,
        nside_exp_grid=10,
        nside_exp_syn=10,
        lmax=4,
        mmax=4,
        interpolate="default",
    ):
        if self.recalculate:
            self.recalculate = False
            self.values = np.array(self.values)
            self.distances = np.array(self.distances)
            self.i_map, self.pixel_dict = gen_interpolation_map(
                self.points, self.values, self.antenna_num, nside_exp_grid
            )
            if interpolate == "increasing_neighborhood":
                self.interp_gain = lambda el, az: hp_interpolator(
                    self.i_map, el, az, n_pix
                )

                # self.interp_gain(el, az)
                np.power(2, nside_exp_syn)
                self.norm_map = self.i_map
            else:
                alms = hp.sphtfunc.map2alm(
                    hp.ma(self.i_map), lmax=lmax, mmax=mmax, iter=10, pol=False
                )
                print(hp.Alm.getlmax(len(alms), mmax=mmax))
                norm_map = hp.sphtfunc.alm2map(
                    alms,
                    np.power(2, nside_exp_syn),
                    lmax=lmax,
                    mmax=mmax,
                    pixwin=False,
                    pol=False,
                )
                norm_map_min = norm_map.min()
                norm_map = norm_map - norm_map_min
                norm_map_max = norm_map.max()
                norm_map = norm_map / norm_map_max
                self.norm_map = norm_map
                # self.interp_gain = lambda el, az: (hp_spheric_harmonics(alms, el, az, mmax=mmax)-norm_map_min)/norm_map_max
                self.interp_gain = lambda el, az: self.norm_map[
                    hp.pixelfunc.ang2pix(
                        np.power(2, nside_exp_syn), np.pi / 2 - el.to_rad(), az.to_rad()
                    )
                ]
        return self.interp_gain(el, az)

    def to_json(self, filename):
        import json

        ret = json.dumps(
            {
                "antenna_num": self.antenna_num,
                "sv": self.sv,
                "distances": self.distances,
                "points": self.points,
                "values": self.values,
            },
            indent=4,
            separators=(",", ": "),
        )
        f = open(filename, "w")
        f.write(ret)
        f.close()

    def to_db(self, utc_date, db_file=None, table="gps_signals"):
        conn, sql = db_connect(db_file, table=table)

        db_remove_antenna_measurements(
            conn, sql, utc_date, self.antenna_num, table=table
        )
        cursor = conn.cursor()
        for p, v, sv, distance in zip(
            self.points, self.values, self.sv, self.distances
        ):
            el, az = p
            correlation = v
            db_insert_row(
                cursor,
                sql,
                utc_date,
                self.antenna_num,
                sv,
                el,
                az,
                correlation,
                distance,
                table=table,
            )

        conn.commit()
        conn.close()

    @classmethod
    def from_json(self, filename):
        import json

        f = open(filename, "r")
        data = f.read()
        x = json.loads(data)
        f.close()
        ret = EmpiricalAntenna(x["antenna_num"])
        ret.points = x["points"]
        ret.values = x["values"]
        ret.sv = x["sv"]
        ret.distances = x["distances"]
        ret.recalculate = True
        return ret

    @classmethod
    def from_data(self, antenna_num, el, az, gain, dates, svs):
        ret = EmpiricalAntenna(antenna_num)
        print("Ant # {}. Data has {} entries".format(antenna_num, len(el)))

        points = []
        sv = []
        values = []
        times = []
        for _el, _az, _gain, _date, _sv in zip(el, az, gain, dates, svs):
            if ~np.isnan(_gain):
                points.append((_el, _az))
                values.append(_gain)
                times.append(_date)
                sv.append(_sv)
        ret.times = times
        ret.points = points
        ret.values = values
        ret.sv = sv

        ret.recalculate = True
        return ret

    @classmethod
    def from_db(self, antenna_num, db_file=None, table="gps_signals"):
        ret = EmpiricalAntenna(antenna_num)
        conn, sql = db_connect(db_file, table=table)
        c = conn.cursor()
        c.execute(
            sql(
                "SELECT el, az, correlation, date, sv, distance FROM "
                + table
                + " WHERE (antenna=%(ph)s)"
            ),
            (antenna_num,),
        )
        # c.execute(sql("SELECT el, az, correlation, date FROM gps_signals WHERE (antenna=%(ph)s) AND el>15 AND correlation<6 AND date<%(ph)s"), (antenna_num, "2013-11-25"))
        # c.execute(sql("SELECT el, az, correlation, date FROM gps_signals WHERE (antenna=%(ph)s) AND date<%(ph)s"), (antenna_num, "2013-11-25"))
        pval = c.fetchall()
        conn.close()
        print("Database has {} entries".format(len(pval)))
        points = []
        sv = []
        values = []
        times = []
        distances = []
        for p in pval:
            if ~np.isnan(p[2]):
                points.append((p[0], p[1]))
                values.append(p[2])
                times.append(p[3])
                sv.append(p[4])
                distances.append(p[5])
        ret.times = times
        ret.points = points
        ret.values = values
        ret.sv = sv
        ret.distances = distances
        ret.recalculate = True
        return ret
