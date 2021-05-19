"""Copyright (C) Max Scheel 2013. All rights reserved"""

from tart.util import angle

# from tart.imaging import sun
from tart.imaging import radio_source
from tart.imaging import gps_satellite
from tart.imaging import location

from tart.simulation import simulation_source

import numpy as np
import healpy as hp
import matplotlib.pyplot as plt

import requests
import json
import datetime


def get_L1_srcs(ts=None):
    """
    In order to reduce the number of requests to the catalog it is adviseable to establish a local nginx proxy:
    apt-get install nginx
    vim /etc/nginx/sites-available/default

    proxy_cache_path /data/nginx/cache levels=1:2 keys_zone=my_cache:10m max_size=10g;
    server {
    ...
          location /catalog/ {
                proxy_cache my_cache;
                proxy_cache_methods GET HEAD POST;
                proxy_ignore_headers Cache-Control;
                proxy_cache_valid any 30m;
                proxy_pass https://tart.elec.ac.nz/catalog/;
          }
    """
    # catalog_server = "https://tart.elec.ac.nz/catalog"

    catalog_server = "http://localhost/catalog"
    if ts is None:
        ts = datetime.datetime.utcnow()
        r = requests.get(
            "{}/catalog?lat=-45.851820&lon=170.545448&date={}".format(
                catalog_server, ts.isoformat()
            )
        )
    else:
        r = requests.get(
            "{}/catalog?lat=-45.851820&lon=170.545448&date={}".format(
                catalog_server, ts.isoformat()
            )
        )
    return np.array(json.loads(r.text))


class Skymodel(object):
    """Ensemble of sources and their visibilities"""

    def __init__(
        self,
        n_sources,
        location,
        sun_str=2.0e5,
        sat_str=5.01e6,
        gps=True,
        l1_catalog=False,
        l1_elevation_threshold=30,
        l1_allowed=[],
        thesun=False,
        known_cosmic=True,
    ):
        self.n_sources = n_sources
        self.source_list = []
        self.known_objects = []
        self.gps_ants = []
        self.location = location

        self.l1_catalog = l1_catalog
        self.l1_elevation_threshold = l1_elevation_threshold
        self.l1_allowed = l1_allowed

        self.sun_str = sun_str
        self.sat_str = sat_str
        self.gps = gps
        self.thesun = thesun
        self.known_cosmic = known_cosmic

        self.el_threshold = 0

        if self.thesun:
            self.add_src(sun.Sun(jy=sun_str))
        if self.gps:
            for i in range(32):
                sv = gps_satellite.GpsSatellite(
                    i + 1, location=self.location, jy=sat_str
                )
                self.add_src(sv)
        # if self.known_cosmic:
        #  for src in radio_source.BrightSources:
        #    self.add_src(src)
        # for _ in range(self.n_sources):
        # ra = angle.from_dms(np.random.uniform(0., 360.))
        # dec = angle.asin(np.random.uniform(-1., 1))
        # cs = radio_source.CosmicSource(ra, dec)
        #  self.add_src(cs)

    def gen_beam(
        self, utc_date_init, utc_date_obs, config, radio, az_deg=20.0, el_deg=80.0
    ):
        """ Generate point source with constant at RA and DEC according to given az and el at time utc_date_init"""
        sources = []
        ra, dec = location.get_loc(config).horizontal_to_equatorial(
            utc_date_init, angle.from_dms(el_deg), angle.from_dms(az_deg)
        )
        src = radio_source.CosmicSource(ra, dec)
        el, az = src.to_horizontal(location.get_loc(config), utc_date_obs)
        sources.append(
            simulation_source.SimulationSource(
                amplitude=1.0,
                azimuth=az,
                elevation=el,
                sample_duration=radio.n_samples / radio.ref_freq,
            )
        )
        return sources

    def get_cum_src_flux(self, utc_date):
        """Return cumulative flux as list over all sources"""
        cumulativ_src_flux = []
        for k in range(len(self.known_objects) + 1):
            cumulativ_src_flux.append(
                np.array([src.jansky(utc_date) for src in self.known_objects[:k]]).sum()
            )
        return cumulativ_src_flux

    def get_int_src_flux(self, utc_date):
        """Return cumulative flux"""
        return self.get_cum_src_flux(utc_date)[-1]

    def gen_photons_per_src(self, utc_date, radio, config, n_samp=1):
        """ Generate n_samp photons per source"""
        sources = []
        # for src in self.known_objects:
        if self.l1_catalog:
            for src in get_L1_srcs(utc_date):
                if src["el"] > self.l1_elevation_threshold:
                    l = len(self.l1_allowed)
                    if (l == 0) or (src["name"] in self.l1_allowed["names"]):
                        if l != 0:
                            s_str = self.l1_allowed["strength_log10"][
                                self.l1_allowed["names"].index(src["name"])
                            ]
                            # print(s_str, type(s_str))
                            amplitude = np.power(10, s_str)
                        else:
                            amplitude = 1.0
                        print(src["name"], src["az"], src["el"])
                        sources.append(
                            simulation_source.SimulationSource(
                                r=src["r"],
                                amplitude=amplitude,
                                azimuth=angle.from_dms(src["az"]),
                                elevation=angle.from_dms(src["el"]),
                                sample_duration=radio.n_samples / radio.ref_freq,
                            )
                        )
                        # print('here')

        for src in self.known_objects:
            print("extra", src)
            # for src in self.get_src_objects(location.get_loc(config), utc_date):
            ra, declination = src.radec(utc_date)
            dx, dy = np.random.multivariate_normal(
                [0.0, 0.0], np.identity(2) * np.power(src.width, 2.0), n_samp
            ).T
            for j in range(n_samp):
                el, az = location.get_loc(config).equatorial_to_horizontal(
                    utc_date,
                    ra + angle.from_dms(dx[j]),
                    declination + angle.from_dms(dy[j]),
                )
                sources.append(
                    simulation_source.SimulationSource(
                        r=src.r,
                        amplitude=src.jansky(utc_date)
                        / self.get_int_src_flux(utc_date)
                        * 1.0
                        / n_samp,
                        azimuth=az,
                        elevation=el,
                        sample_duration=radio.n_samples / radio.ref_freq,
                    )
                )
        print(len(sources))
        return sources

    def gen_n_photons(self, config, utc_date, radio, n=10):
        """ Generate a total of n photons. Sources with more jansky will contribute more photons"""
        cumulativ_src_flux = self.get_cum_src_flux(utc_date)
        int_src_flux = cumulativ_src_flux[-1]
        rel_noise_flux = 0.0
        tot_flux = int_src_flux * (1.0 + rel_noise_flux)
        src_identifier = np.random.uniform(0.0, tot_flux, n)
        hi, _ = np.histogram(src_identifier, bins=cumulativ_src_flux)
        ret = []
        for src_num, count in enumerate(hi):
            src = self.known_objects[src_num]
            # Assume the sky is flat.
            # this will also cause problems at problems at boundaries of dec.
            dx, dy = np.random.multivariate_normal(
                [0.0, 0.0], np.identity(2) * np.power(src.width, 2.0), count
            ).T
            ra, declination = src.radec(utc_date)
            for j in range(count):
                el, az = location.get_loc(config).equatorial_to_horizontal(
                    utc_date,
                    ra + angle.from_dms(dx[j]),
                    declination + angle.from_dms(dy[j]),
                )
                ret.append(
                    simulation_source.SimulationSource(
                        amplitude=1.0 / n,
                        azimuth=az,
                        elevation=el,
                        sample_duration=radio.n_samples / radio.ref_freq,
                    )
                )
        return ret

    def set_el_threshold(self, threshold=0):
        self.el_threshold = threshold

    def get_src_objects(self, location, utc_date, threshold=None, debug=False):
        """optional argument threshold defines elevation threshold in deg. 0 = horizon. 90 = zenith"""
        if threshold is None:
            threshold = self.el_threshold
        ret = []
        for src in self.known_objects:
            try:
                el, az = src.to_horizontal(location=location, utc_date=utc_date)
                if el.to_degrees() > threshold:
                    ret.append(src)
            except:
                print("No position for", src)
                self.del_src(src)
        return ret

    def add_src(self, src):
        """Add source to known_objects"""
        self.known_objects.append(src)

    def del_src(self, src):
        """Remove source from known_objects"""
        self.known_objects.remove(src)

    def get_state_vector(self):
        """Return state vector"""
        state_vector = np.zeros(self.n_sources * 4)
        for i, source in enumerate(self.source_list):
            state_vector[i + (0 * self.n_sources)] = source.skyloc.ra.to_degrees()
            if source.skyloc.dec.to_degrees() > 90.0:
                state_vector[i + (1 * self.n_sources)] = (
                    source.skyloc.dec.to_degrees() - 360.0
                )
            else:
                state_vector[i + (1 * self.n_sources)] = source.skyloc.dec.to_degrees()
            state_vector[i + (2 * self.n_sources)] = source.flux
            state_vector[i + (3 * self.n_sources)] = source.width
        return [
            self.sun_str,
            self.sat_str,
            self.gps,
            self.thesun,
            self.known_cosmic,
            self.location,
            state_vector,
        ]

    def get_src_positions(self, location, utc_date):
        """ Save way of getting lists of el and az for all known_objects"""
        l_el = []
        l_az = []
        l_name = []
        for src in self.known_objects:
            try:
                el, az = src.to_horizontal(location, utc_date)
                if el.to_rad() > 0:
                    l_el.append(el.to_rad())
                    l_az.append(az.to_rad())
                    l_name.append(str(src))
                    # l_name.append('%s %2.2f %1.1e' % (str(src), el.to_degrees(), src.jansky(utc_date)))
            except:
                print("no position", src)
        return l_el, l_az, l_name

    def true_image(self, location, utc_date, nside):
        """Plot current sky."""

        l_el, l_az, l_name = self.get_src_positions(location, utc_date)

        m = np.zeros(hp.nside2npix(nside)) * hp.UNSEEN
        th = -np.array(l_el) + np.pi / 2.0
        l_phi = -np.array(l_az)
        pix = hp.pixelfunc.ang2pix(nside, th, l_phi)
        m[pix] = 1  # [src.jansky(utc_date) for src in self.known_objects]
        m = hp.sphtfunc.smoothing(m, fwhm=3.0 * np.pi / 180.0)
        # plt.figure()
        # hp.visufunc.orthview(m, title="", rot=(0,90,0), )
        return m

    def true_image_overlay(self, location, utc_date):
        """Plot current sky.
        Theta is colatitude and measured from North pole. 0 .. pi
         (straight up) |  el:  pi/2   | theta 0
         (horizon)     |  el:  0      | theta pi/2
        Th = pi/2 - el

        flip : {'astro', 'geo''}, optional
        Defines the convention of projection : 'astro'' (default, east towards left, west towards right) or 'geo' (east towards right, west towards left)
        """
        l_el, l_az, l_name = self.get_src_positions(location, utc_date)

        th = np.pi / 2.0 - np.array(l_el)
        l_phi = -np.array(l_az)
        # _ = [hp.projtext(i, j, n, rot=(0,90,0), color='black', weight='light', ha='left', va='center') for i, j, n in zip(th, l_phi, l_name)]
        _ = [
            hp.projtext(
                i,
                j,
                n,
                rot=(0, 90, 0),
                color="gray",
                alpha=0.8,
                weight="bold",
                ha="center",
                va="bottom",
            )
            for i, j, n in zip(th, l_phi, l_name)
        ]
        _ = [
            hp.projscatter(i, j, rot=(0, 90, 0), color="black", alpha=1.0, s=25)
            for i, j, n in zip(th, l_phi, l_name)
        ]
        _ = [
            hp.projscatter(i, j, rot=(0, 90, 0), color="white", alpha=1.0, s=10)
            for i, j, n in zip(th, l_phi, l_name)
        ]

        hp.projtext(np.pi / 2, 0.0, "N", rot=(0, 90, 0), va="top", ha="center")
        hp.projtext(np.pi / 2, -np.pi / 2.0, "E", rot=(0, 90, 0), va="center")
        hp.projtext(np.pi / 2, -np.pi, "S", rot=(0, 90, 0), ha="center")
        hp.projtext(
            np.pi / 2, -np.pi * 3.0 / 2.0, "W", rot=(0, 90, 0), ha="right", va="center"
        )

    def true_FFT_overlay(self, utc_date, nw, num_bin):
        """Plot current sky.
        Theta is colatitude and measured from North pole. 0 .. pi
         (straight up) |  el:  pi/2   | theta 0
         (horizon)     |  el:  0      | theta pi/2
        Th = pi/2 - el

        flip : {'astro', 'geo''}, optional
        Defines the convention of projection : 'astro'' (default, east towards left, west towards right) or 'geo' (east towards right, west towards left)
        """
        from tart.imaging.synthesis import get_max_ang

        max_ang = get_max_ang(nw, num_bin)
        l_el, l_az, l_name = self.get_src_positions(self.location, utc_date)
        l_el, l_az = np.array(l_el), np.array(l_az)

        r = 90.0 - np.degrees(l_el)
        x = r * np.sin(l_az)
        y = r * np.cos(l_az)
        plt.scatter(x, y, color="white", s=50)


def from_state_vector(state):
    """Generate skymodel from state vector"""
    sun_str, sat_str, gps, thesun, known_cosmic, location, state_vector = state
    psky = Skymodel(
        0,
        location=location,
        sun_str=sun_str,
        sat_str=sat_str,
        gps=gps,
        known_cosmic=known_cosmic,
    )
    psky.n_sources = len(state_vector) / 4
    psky.source_list = []

    for i in range(psky.n_sources):
        ra = angle.from_dms(state_vector[i + (0 * psky.n_sources)])
        dec = angle.from_dms(state_vector[i + (1 * psky.n_sources)])
        gs = radio_source.CosmicSource(
            ra,
            dec,
            jy=state_vector[i + (2 * psky.n_sources)],
            width=state_vector[i + (3 * psky.n_sources)],
        )
        psky.source_list.append(gs)
    return psky
