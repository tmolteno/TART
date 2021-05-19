"""
    Image from the HTTP API

    Tim Molteno & Max Scheel 2017-2021.
"""
import os
import dateutil.parser
import astropy.io.fits as pyfits

import numpy as np
import healpy as hp

from tart.operation import settings
from tart.imaging import visibility
from tart.imaging import calibration
from tart.imaging import synthesis
from tart.imaging import elaz


def vis_json_timestamp(vis_json):
    return dateutil.parser.parse(vis_json["timestamp"])


def vis_object_from_json(vis_json, config):
    ts = vis_json_timestamp(vis_json)
    ret = visibility.Visibility(config, ts)
    v_order = []
    bl_order = []
    for v in vis_json["data"]:
        v_order.append(complex(v["re"], v["im"]))
        bl_order.append([v["i"], v["j"]])
    ret.set_visibilities(v_order, bl_order)
    return ret


def vis_calibrated(vis_json, config, gains, phase_offset, flag_list=[]):
    vis = vis_object_from_json(vis_json, config)

    cal_vis = calibration.CalibratedVisibility(vis)
    for f in flag_list:
        cal_vis.flag_antenna(f)

    cal_vis.set_gain(np.arange(24), gains)
    cal_vis.set_phase_offset(np.arange(24), phase_offset)

    return cal_vis, vis.timestamp


def rotate_vis(rot_degrees, cv, reference_positions):
    conf = cv.vis.config

    new_positions = settings.rotate_location(
        rot_degrees, np.array(reference_positions).T
    )
    conf.set_antenna_positions((np.array(new_positions).T).tolist())


def image_from_calibrated_vis(cv, nw, num_bin):
    cal_syn = synthesis.Synthesis_Imaging([cv])

    cal_ift, cal_extent = cal_syn.get_ift(nw=nw, num_bin=num_bin, use_kernel=False)
    # beam = cal_syn.get_beam(nw=nw, num_bin=num_bin, use_kernel=False)
    n_fft = len(cal_ift)
    assert n_fft == num_bin

    bin_width = (max(cal_extent) - min(cal_extent)) / float(n_fft)

    return cal_ift, cal_extent, n_fft, bin_width


def beam_from_calibrated_vis(cv, nw, num_bin):
    """
    Generate a beam (or Point Spread Function (PSF)
    for the antenna array
    """
    cal_syn = synthesis.Synthesis_Imaging([cv])

    return cal_syn.get_beam(nw=nw, num_bin=num_bin, use_kernel=False)


def get_uv_fits(cv):
    cal_syn = synthesis.Synthesis_Imaging([cv])
    return cal_syn.get_uvfits()


def make_image(plt, img, title, num_bins, source_json=None, healpix=False):
    if healpix:
        make_healpix_image(plt, img, title, num_bins, source_json)
    else:
        make_square_image(plt, img, title, num_bins, source_json)


def make_square_image(plt, img, title, num_bins, source_json=None):

    plt.figure(figsize=(8, 6), dpi=num_bins / 6)
    plt.title(title)

    print("Dynamic Range: {}".format(np.max(img)))

    plt.imshow(img, extent=[-1, 1, -1, 1])

    plt.xlim(-1, 1)
    plt.ylim(-1, 1)
    cb = plt.colorbar()

    if source_json is not None:
        src_list = elaz.from_json(source_json, el_limit=20.0, jy_limit=1e4)
        output_list = []
        output_list.append(plt.Circle([0, 0], 1.0, color=(0.1, 0.1, 0.9), fill=False))
        for s in src_list:
            output_list.append(
                plt.Circle(s.get_lm(), 0.03, color=(0.9, 0.2, 0.3), fill=False)
            )
        ax = plt.gca()
        for circle in output_list:
            ax.add_artist(circle)

    # plt.grid()
    plt.xlabel("East-West")
    plt.ylabel("North-South")
    plt.tight_layout()


def make_healpix_image(plt, img, title, num_bins, source_json=None):
    """
    Writes out an image as a healpy image
    """
    nside = hp.pixelfunc.get_min_valid_nside(num_bins * num_bins * 3 / 4)
    npix = hp.nside2npix(nside)

    pixels = np.arange(npix)
    m = np.zeros(npix) + hp.UNSEEN

    window_d = np.degrees(hp.nside2resol(nside))

    for i in pixels:
        theta, phi = hp.pix2ang(nside, i)
        if theta < np.pi / 2:
            el = np.degrees(np.pi / 2 - theta)
            az = np.degrees(phi)
            s = elaz.ElAz(el, az)
            x_min, x_max, y_min, y_max, area = s.get_px_window(
                num_bins, window_deg=window_d
            )
            s_px = img[y_min:y_max, x_min:x_max]

            m[i] = np.sum(s_px) / area

    hp.orthview(m, rot=(0, 90, 180), title=title, xsize=3000, cbar=False, half_sky=True)
    hp.graticule()
    if source_json is not None:
        src_list = elaz.from_json(source_json, el_limit=20.0, jy_limit=1e4)
        output_list = []
        for s in src_list:
            l, m = s.get_lm()
            output_list.append(
                plt.Circle([-l, m], 0.03, color=(0.9, 0.2, 0.3), fill=False)
            )
        ax = plt.gca()
        for circle in output_list:
            ax.add_artist(circle)

    # hp.projplot([float(sp.N(theta_actual)),], [float(sp.N(phi_actual)),], 'ro', rot=(0,90,0))
    # hp.projplot([float(sp.N(theta_actual2)),], [float(sp.N(phi_actual2)),], 'ro', rot=(0,90,0))


def save_fits_image(img, fname, timestamp, out_dir, header_dict={}):
    """
    This method saves a 2D array as a FITS image, and adds the minimum neccessary headers
    to get the image to be processable by MORESANE
    """
    hdu = pyfits.PrimaryHDU(img.astype(np.float32))
    hdulist = pyfits.HDUList([hdu])
    deg_per_pixel = 180.0 / len(img)
    prihdr = hdulist[0].header

    prihdr.set("CTYPE1", "RA---SIN")  # First parameter RA  ,  projection TANgential
    prihdr.set("CDELT1", deg_per_pixel)  # Degrees/pixel
    prihdr.set("CUNIT1", "deg")  # Degrees

    prihdr.set("CTYPE2", "DEC--SIN")  # Second parameter DEC,  projection TANgential
    prihdr.set("CDELT2", deg_per_pixel)  # Degrees/pixel
    prihdr.set("CUNIT2", "deg")  # Degrees

    prihdr.set("CROTA2", 0.00000)  # Rotation in degrees.

    prihdr.set("BMAJ", 0.0)  # Beam Major Axis (degrees)
    prihdr.set("BMIN", 90.0)  # Beam Minor Axis (degrees)
    prihdr.set("BPA", 0.0)  # Beam position angle

    prihdr.set("DATE", timestamp)
    prihdr.set("DATE-OBS", timestamp)

    prihdr.set("TIMESYS", "UTC")
    prihdr.set("INSTRUME", "TART")
    prihdr.set("TELESCOP", "TART")
    prihdr.set("OBSERVER", "CASA simulator")
    prihdr.set("ORIGIN", "tart_tools tart.elec.ac.nz ")

    prihdr.set("OBSRA", 2.889721000000e02)
    prihdr.set("OBSDEC", -7.466052777778e01)
    prihdr.set("OBSGEO-X", 5.111202828133e06)
    prihdr.set("OBSGEO-Y", 2.001309252764e06)
    prihdr.set("OBSGEO-Z", -3.237339358474e06)

    """
    SIMPLE  =                    T /Standard FITS                                   
    BITPIX  =                  -32 /Floating point (32 bit)                         
    NAXIS   =                    4                                                  
    NAXIS1  =                 1024                                                  
    NAXIS2  =                 1024                                                  
    NAXIS3  =                    1                                                  
    NAXIS4  =                    1                                                  
    BSCALE  =   1.000000000000E+00 /PHYSICAL = PIXEL*BSCALE + BZERO                 
    BZERO   =   0.000000000000E+00                                                  
    BTYPE   = 'Intensity'                                                           
    OBJECT  = 'KAT7_1445_1x16_12h'                                                  
                                                                                    
    BUNIT   = 'JY/BEAM '           /Brightness (pixel) unit                         
    EQUINOX =   2.000000000000E+03                                                  
    LONPOLE =   1.800000000000E+02                                                  
    LATPOLE =  -7.466052777778E+01                                                  
    PC001001=   1.000000000000E+00                                                  
    PC002001=   0.000000000000E+00                                                  
    PC003001=   0.000000000000E+00                                                  
    PC004001=   0.000000000000E+00                                                  
    PC001002=   0.000000000000E+00                                                  
    PC002002=   1.000000000000E+00                                                  
    PC003002=   0.000000000000E+00                                                  
    PC004002=   0.000000000000E+00                                                  
    PC001003=   0.000000000000E+00                                                  
    PC002003=   0.000000000000E+00                                                  
    PC003003=   1.000000000000E+00                                                  
    PC004003=   0.000000000000E+00                                                  
    PC001004=   0.000000000000E+00                                                  
    PC002004=   0.000000000000E+00                                                  
    PC003004=   0.000000000000E+00                                                  
    PC004004=   1.000000000000E+00                                                  
    CTYPE1  = 'RA---SIN'                                                            
    CRVAL1  =   2.889721000000E+02                                                  
    CDELT1  =  -1.953125000000E-03                                                  
    CRPIX1  =   5.130000000000E+02                                                  
    CUNIT1  = 'deg     '                                                            
    CTYPE2  = 'DEC--SIN'                                                            
    CRVAL2  =  -7.466052777778E+01                                                  
    CDELT2  =   1.953125000000E-03                                                  
    CRPIX2  =   5.130000000000E+02                                                  
    CUNIT2  = 'deg     '                                                            
    CTYPE3  = 'STOKES  '                                                            
    CRVAL3  =   1.000000000000E+00                                                  
    CDELT3  =   1.000000000000E+00                                                  
    CRPIX3  =   1.000000000000E+00                                                  
    CUNIT3  = '        '                                                            
    CTYPE4  = 'FREQ    '                                                            
    CRVAL4  =   1.445000000000E+09                                                  
    CDELT4  =   1.600000000000E+07                                                  
    CRPIX4  =   1.000000000000E+00                                                  
    CUNIT4  = 'HZ      '                                                            
    PV2_1   =   0.000000000000E+00                                                  
    PV2_2   =   0.000000000000E+00                                                  
    RESTFREQ=   1.445000000000E+09 /Rest Frequency (Hz)                             
    ALTRVAL =  -0.000000000000E+00 /Alternate frequency reference value             
    ALTRPIX =   1.000000000000E+00 /Alternate frequency reference pixel             
    VELREF  =                    3 /1 LSR, 2 HEL, 3 OBS, +256 Radio                 
    COMMENT casacore non-standard usage: 4 LSD, 5 GEO, 6 SOU, 7 GAL                 
    TELESCOP= 'MeerKAT '                                                            
    OBSERVER= 'CASA simulator'                                                      
    DATE-OBS= '2012-03-21T00:00:00.000000'                                          
    TIMESYS = 'UTC     '                                                            
    OBSRA   =   2.889721000000E+02                                                  
    OBSDEC  =  -7.466052777778E+01                                                  
    OBSGEO-X=   5.111202828133E+06                                                  
    OBSGEO-Y=   2.001309252764E+06                                                  
    OBSGEO-Z=  -3.237339358474E+06                                                  
    OBJECT  = 'KAT7_1445_1x16_12h'                                                  
    TELESCOP= 'MeerKAT '                                                            
    INSTRUME= 'MeerKAT '                                                            
    DISTANCE=   0.000000000000E+00                                                  
    DATE    = '2013-01-15T15:21:51.222000' /Date FITS file was written              
    ORIGIN  = 'CASA casacore alma-evla '           
    """
    for k in list(header_dict.keys()):
        prihdr.set(k, header_dict[k])
    hdulist.writeto(os.path.join(out_dir, fname))
