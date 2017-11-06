#
# A class the encapsulates a location on earth. Does all the necessary frame-of-reference
# conversions.
#
# Copyright (c) Tim Molteno 2013. tim@elec.ac.nz
#

import math
import numpy as np

from tart.util import angle
from tart.imaging import tart_util

def get_loc(Settings):
    return Location(angle.from_dms(Settings.get_lat()), angle.from_dms(Settings.get_lon()), Settings.get_alt())


def JulianDay (utc_date):
    jhr =utc_date.hour + utc_date.minute / 60.0 + utc_date.second / 3600.0
    day = utc_date.day
    month = utc_date.month
    year = utc_date.year

    if (month<=2):
      month=month+12
      year=year-1
    return (int)(365.25*year) + (int)(30.6001*(month+1)) - 15 + 1720996.5 + day + jhr/24.0


#''' return the greenwich sidereal time'''
#def gst(utc_date):
    #jd = JulianDay(utc_date)
    #d = jd - 2451545.0
    #t = int(d / 36525)

    #gmst = 18.697374558 + 24.06570982441908*d

    ## now correct for precession of equioxes
    #epsilon = 23.4393 - 0.0000004*d
    #l = 280.47 + 0.98565*d
    #omega = 125.04 - 0.052954*d
    #dphi = -0.000319*math.sin(angle.deg_to_rad(omega)) - 0.000024*math.sin(angle.deg_to_rad(2.0*l))
    #eqeq = dphi * math.cos(angle.deg_to_rad(epsilon))

    #return angle.from_hours(gmst + eqeq)

# Convert ECI (Earth-Centered-Inertial) to ECEF coordinates.
def eci_to_ecef(utc_date, x_in, y_in, z_in):
    ''' cos(theta) -sin(theta)  0
        sin(theta)  cos(theta)  0
        0           0           1
    '''
    # Rotate x and y by the hour angle
    theta = Location.GST(utc_date)

    return [x_in*theta.cos() - y_in*theta.sin(), y_in*theta.cos() + x_in*theta.sin(), z_in]

# Convert ECEF to ECI (Earth-Centered-Inertial)
def ecef_to_eci(utc_date, x_in, y_in, z_in):

    # Undo Rotate x and y by the hour angle
    theta = Location.GST(utc_date)

    return [x_in*theta.cos() + y_in*theta.sin(), y_in*theta.cos() - x_in*theta.sin(), z_in]

class Location:
    R_EARTH = 6378137.0 # earth semimajor axis in meters
    F_RECIP = 1.0/298.257223563 # reciprocal flattening
    E2 = 2.0*F_RECIP - F_RECIP*F_RECIP # eccentricity squared

    def __init__(self, lat, lon, alt):
        self.lat = lat # requires init with lat and lon as angle objects (NEW)
        self.lon = lon # requires init with lat and lon as angle objects (NEW)
        self.alt = alt

    def __repr__(self):
        return "[lat=%f, lon=%f, alt=%f]" % (self.latitude_deg(), self.longitude_deg(), self.alt)

    def longitude_deg(self):
        return angle.wrap_180(self.lon.to_degrees())

    def latitude_deg(self):
        return self.lat.to_degrees()

    def altitude(self):
        return self.alt

    def get_XYZ(self, e, n, u):
        '''Return the location in terms of the XYZ (non-earth fixed coordinate system).
             The X axis starts at the earths center and points through (HA = 0, decl = 0)
             The Y axis points at 90 degrees to X and in the plane of the equator (HA = -90, decl=0)
             Z points through the north celestial pole (decl = 90)'''
        ecef0 = self.get_ecef() # ECEF of origin
        # Now create a new location offset by ENU
        d_lon = angle.from_rad((e/self.R_EARTH)*self.lat.sin())
        d_lat = angle.from_rad(n/self.R_EARTH)

        loc2 = Location((self.lat + d_lat), (self.lon + d_lon), self.alt + u)
        ecef2 = loc2.get_ecef()

        return np.array(ecef2) - np.array(ecef0)


    def get_ecef(self):
        sinlat = self.lat.sin()
        chi = math.sqrt(1.0 - Location.E2*sinlat*sinlat)
        coslat = self.lat.cos()
        achih = (Location.R_EARTH/chi + self.alt)
        x = achih*coslat*self.lon.cos()
        y = achih*coslat*self.lon.sin()
        z = (Location.R_EARTH*(1.0-Location.E2)/chi + self.alt)*sinlat

        return [x,y,z]

     # ECEF to ENU
    def ecef_to_enu(self, x_in, y_in, z_in):
        lon = self.lon
        lat = self.lat
        e = -x_in*lon.sin()                    + y_in*lon.cos()
        n = -x_in*lon.cos()*lat.sin() - y_in*lon.sin()*lat.sin() + z_in*lat.cos()
        u =    x_in*lon.cos()*lat.cos() + y_in*lon.sin()*lat.cos() + z_in*lat.sin()
        return np.array([e,n,u])

    def get_ecef_delta_from_enu(self,e_in,n_in,u_in):
        # FIXME: definitely TEST ENU_TO_ECEF
        lat = self.lat
        lon = self.lon
        x = -lon.sin() * e_in - lon.cos()*lat.sin()*n_in + lon.cos()*lat.cos()*u_in
        y =    lon.cos() * e_in - lon.sin()*lat.sin()*n_in + lon.sin()*lat.cos()*u_in
        z = lat.cos() * n_in + lat.sin() * u_in
        return [x,y,z]

    # http://www.navipedia.net/index.php/Transformations_between_ECEF_and_ENU_coordinates
    def ecef_to_horizontal(self, x_in, y_in, z_in):

        ex,ey,ez = self.get_ecef() # My position in ECEF

        rx,ry,rz = [x_in - ex, y_in - ey, z_in - ez]
        enu = self.ecef_to_enu(rx,ry,rz)

        r = np.sqrt(enu.dot(enu))
        rho = enu / r

        el = angle.asin(rho[2])

        n = rho[1] #n
        e = rho[0] #e

        az = angle.atan2(e, n)
        if az.to_degrees() < 0.:
            az = angle.from_dms(360. + az.to_degrees())


        return [r, el, az]

    @staticmethod
    def GST(utc_date):
        ''' Return the Greenwich Sidereal Time'''
        JD = tart_util.JulianDay(utc_date)
        D = JD - 2451545.0
        T = int(D / 36525)

        GMST = 18.697374558 + 24.06570982441908*D

        # Now correct for precession of equioxes
        epsilon = 23.4393 - 0.0000004*D
        L = 280.47 + 0.98565*D
        omega = 125.04 - 0.052954*D
        dphi = -0.000319*math.sin(angle.deg_to_rad(omega)) - 0.000024*math.sin(angle.deg_to_rad(2.0*L))
        eqeq = dphi * math.cos(angle.deg_to_rad(epsilon))

        return angle.from_hours(GMST + eqeq)

    def LST(self, utc_date):
        return Location.GST(utc_date) + self.lon

    ''' This is the Greenwich Hour Angle (GHA)'''
    def GHA(self, utc_date, ra):
        return Location.GST(utc_date) - ra

    def horizontal_to_LHA(self, utc_date, el, az):
        phi_0 = self.lat.to_rad()

        tan_h = az.sin() / (az.cos() * math.sin(phi_0) + el.tan() * math.cos(phi_0))
        local_hour_angle = angle.atan(tan_h)
        return local_hour_angle

    ''' This is the Local Hour Angle (LHA)'''
    def LHA(self, utc_date, ra):
        return self.LST(utc_date) - ra

    # Convert RA/Decl to Elevation Azimuth this location
    def equatorial_to_horizontal(self, utc_date, ra, dec):
        # [Peter Duffett-Smith, Jonathan_Zwart] Practical Astronomy with calculator and spreadsheet
        lha = self.LHA(utc_date,ra)
        lat = self.lat

        el = angle.asin(dec.sin()*lat.sin() + dec.cos()*lat.cos()*lha.cos())

        az = angle.atan2((-lha.sin())*dec.cos(), (dec.sin()-lat.sin()*el.sin())/lat.cos())

        if az.to_degrees() < 0.:
            az = angle.from_dms(360. + az.to_degrees())

        return el, az

    ''' Convert an azimuth and elevation to RA/Decl
            Useful for looking straight up, and working out the RA/Declination
            Return RA , Decl (in degrees)
    '''

    def horizontal_to_equatorial(self, utc_date, el, az):
        lat = self.lat
        dec_sin = (el.sin() * lat.sin()) + (el.cos() * lat.cos() * az.cos())
        dec = angle.asin(dec_sin)

        LST = self.LST(utc_date)

        H = angle.atan2(-1.*el.cos()*lat.cos()*az.sin(), (el.sin()-(lat.sin()*dec_sin)))

        ra = LST - H

        return ra.to_ra(), dec.to_declination()


'''Convenient helper function for the location of the Physics Department Roof'''
Dunedin = Location(lat=angle.from_dms(-45.86391200), lon=angle.from_dms(170.51348452), alt=46.5)

'''Convenient helper function for the location of the rural TART'''
Dunedin_Farm = Location(lat=angle.from_dms(-45.851868), lon=angle.from_dms(170.545558), alt=266.5)

'''Convenient helper function for somewhere cold and damp in the far north'''
Aachen = Location(lat=angle.from_dms(50.778), lon=angle.from_dms(6.086), alt=46.5)
