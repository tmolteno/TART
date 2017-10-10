"""
    Image from the HTTP API

    Tim Molteno & Max Scheel 2017.
"""
import os
import dateutil.parser
import astropy.io.fits as pyfits

import numpy as np

from tart.operation import settings
from tart.imaging import visibility
from tart.imaging import calibration
from tart.imaging import synthesis
from tart.imaging import elaz


def vis_json_timestamp(vis_json):
    return dateutil.parser.parse(vis_json['timestamp'])

def vis_object_from_json(vis_json, config):
    ts = vis_json_timestamp(vis_json)
    ret = visibility.Visibility(config, ts)
    v_order = []
    bl_order = []
    for v in vis_json['data']:
        v_order.append(complex(v['re'], v['im']))
        bl_order.append([v['i'], v['j']])
    ret.set_visibilities(v_order, bl_order)
    return ret

def vis_calibrated(vis_json, config, gains, phase_offset, flag_list=[]):
    v = vis_object_from_json(vis_json, config)

    cv = calibration.CalibratedVisibility(v)
    for f in flag_list:
        cv.flag_antenna(f)

    cv.set_gain(np.arange(24), gains)
    cv.set_phase_offset(np.arange(24), phase_offset)

    return cv, v.timestamp

def rotate_vis(rot_degrees, cv, reference_positions):
    conf = cv.vis.config

    new_positions = settings.rotate_location(rot_degrees, np.array(reference_positions).T)
    conf.set_antenna_positions((np.array(new_positions).T).tolist())


def image_from_calibrated_vis(cv, nw, num_bin):
    cal_syn = synthesis.Synthesis_Imaging([cv])

    cal_ift, cal_extent = cal_syn.get_ift(nw=nw, num_bin=num_bin, use_kernel=False)
    beam = cal_syn.get_beam(nw=nw, num_bin=num_bin, use_kernel=False)
    n_fft = len(cal_ift)
    assert n_fft == num_bin

    bin_width = (max(cal_extent)-min(cal_extent))/float(n_fft)

    return cal_ift, cal_extent, n_fft, bin_width

def beam_from_calibrated_vis(cv, nw, num_bin):
    """
    Generate a beam (or Point Spread Function (PSF)
    for the antenna array
    """
    cal_syn = synthesis.Synthesis_Imaging([cv])

    return cal_syn.get_beam(nw=nw, num_bin=num_bin, use_kernel=False)

def get_uv_fits(cv, fname):
    cal_syn = synthesis.Synthesis_Imaging([cv])
    return cal_syn.get_uvfits(fname)

def save_image(plt, ift_scaled, time_repr, nw, num_bins, out_dir, source_json, show=False):

    plt.figure(figsize=(8, 6), dpi=num_bins/6)
    plt.title(time_repr)

    print("Dynamic Range: {}".format(np.max(ift_scaled)))

    plt.imshow(ift_scaled, extent=[-1, 1, -1, 1])

    plt.xlim(-1, 1)
    plt.ylim(-1, 1)
    cb = plt.colorbar()

    if source_json is not None:
        src_list = elaz.from_json(source_json, el_limit=20.0, jy_limit=1e4)
        output_list = []
        output_list.append(plt.Circle([0, 0], 1.0, color=(0.1, 0.1, 0.9), fill=False))
        for s in src_list:
            output_list.append(plt.Circle(s.get_lm(), 0.03, color=(0.9, 0.2, 0.3), fill=False))
        ax = plt.gca()
        for circle in output_list:
            ax.add_artist(circle)

    #plt.grid()
    plt.xlabel('East-West')
    plt.ylabel('North-South')
    plt.tight_layout()
    if show:
        plt.show()
    else:
        fname = 'tart_image_{}.png'.format(time_repr)
        plt.savefig(os.path.join(out_dir, fname))
        print("Generating {}".format(fname))


def save_fits_image(img, fname, out_dir, header_dict={}):
    """
    This method saves a 2D array as a FITS image, and adds the minimum neccessary headers
    to get the image to be processable by MORESANE
    """
    hdu = pyfits.PrimaryHDU(img)
    hdulist = pyfits.HDUList([hdu])
    deg_per_pixel = 180.0 / len(img)
    prihdr = hdulist[0].header

    prihdr.set('CTYPE1', 'RA---TAN')     # First parameter RA  ,  projection TANgential
    prihdr.set('CTYPE2', 'DEC--TAN')     # Second parameter DEC,  projection TANgential
    prihdr.set('CDELT1', deg_per_pixel)  # Degrees/pixel
    prihdr.set('CDELT2', deg_per_pixel)  # Degrees/pixel
    prihdr.set('CROTA2', 0.00000)        # Rotation in degrees.
    prihdr.set('BMAJ', 0.0)              # Beam Major Axis (degrees)
    prihdr.set('BMIN', 90.0)             # Beam Minor Axis (degrees)
    prihdr.set('BPA', 0.0)               # Beam position angle
    for k in header_dict.keys():
        prihdr.set(k, header_dict[k])
        
    hdulist.writeto(os.path.join(out_dir, fname))

def save_healpix_image(plt, ift_scaled, time_repr, num_bins, out_dir, source_json, show=False):
    """
    Writes out an image as a healpy image
    """
    import healpy as hp
    nside = hp.pixelfunc.get_min_valid_nside(num_bins*num_bins*3/4)
    npix = hp.nside2npix(nside)

    pixels = np.arange(npix)
    m = np.zeros(npix) + hp.UNSEEN

    window_d = np.degrees(hp.nside2resol(nside))

    for i in pixels:
        theta, phi = hp.pix2ang(nside, i)
        if (theta < np.pi / 2):
            el = np.degrees(np.pi/2 - theta)
            az = np.degrees(phi)
            s = elaz.ElAz(el, az)
            x_min, x_max, y_min, y_max, area = s.get_px_window(num_bins, window_deg=window_d)
            s_px = ift_scaled[y_min:y_max, x_min:x_max]

            m[i] = np.sum(s_px)/area

    hp.orthview(m, rot=(0, 90, 180), title=time_repr, xsize=3000, cbar=False, half_sky=True)
    hp.graticule()
    
    if source_json is not None:
        src_list = elaz.from_json(source_json, el_limit=20.0, jy_limit=1e4)
        output_list = []
        for s in src_list:
            l, m = s.get_lm()
            output_list.append(plt.Circle([-l, m], 0.03, color=(0.9, 0.2, 0.3), fill=False))
        ax = plt.gca()
        for circle in output_list:    
            ax.add_artist(circle)

    #hp.projplot([float(sp.N(theta_actual)),], [float(sp.N(phi_actual)),], 'ro', rot=(0,90,0))
    #hp.projplot([float(sp.N(theta_actual2)),], [float(sp.N(phi_actual2)),], 'ro', rot=(0,90,0))
    if show:
        plt.show()
    else:
        fname = 'tart_image_{}.png'.format(time_repr)
        plt.savefig(os.path.join(out_dir, fname))
        print("Generating {}".format(fname))
