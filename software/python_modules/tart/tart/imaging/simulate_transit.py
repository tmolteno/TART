#!/usr/bin/python
# Simulate the known radio sources during an entire day's observing
# Calculate visibilities and compare them to the observed visibilities
#
# Copyright (c) Tim Molteno 2013-2019. tim@elec.ac.nz
#
import traceback
import argparse
import math
import utc

import numpy as np
from multiprocessing import Pool


import datetime

import matplotlib.pyplot as plt
from tart.util import angle

import forward_map

from .visibility import *
from . import location
from ..operation import observation


def process_file(
    dirname, fname, orientation_degrees, spacing, sample_duration, simulate_vis=True
):
    try:
        fpath = os.path.join(dirname, fname)
        nobs = observation.Observation_Load(fpath)
        date = nobs.timestamp.date()
        sec = (
            nobs.timestamp - datetime.datetime.combine(date, datetime.time.min)
        ).total_seconds()

        if simulate_vis == True:
            utc_date = nobs.timestamp.replace(tzinfo=utc.UTC())
            antenna_locations = antenna_locations = nobs.ant_positions

            sky, baselines, visibility = forward_map.forward_map(
                utc_date,
                location.Dunedin,
                horizon=5.0,
                antenna_locations=antenna_locations,
                sample_duration=sample_duration,
            )  # Remove things below an elevation of five degrees
        else:
            sky = False
            baselines = False
            visibility = [False]

        hours = sec / 3600.0
        v = Visibility(nobs)
        return date, hours, v, visibility[0]
    except Exception as inst:
        print(type(inst))  # the exception instance
        print(inst.args)  # arguments stored in .args
        print(inst)
        print(sample_duration)
        print(traceback.format_exc())
        return None, None, None, None
    except KeyboardInterrupt:
        print(inst)
        return None, None, None, None


def get_correlation(
    dirname, orientation_degrees, spacing, sample_duration, simulate_vis=True
):
    files = sorted(os.listdir(dirname))
    vis = []
    simvis = []
    times = []

    if True:
        p = Pool()

        resultList = []

        try:
            for fname in files:
                if "data.pkl" in fname:
                    resultList.append(
                        p.apply_async(
                            process_file,
                            (
                                dirname,
                                fname,
                                orientation_degrees,
                                spacing,
                                sample_duration,
                                simulate_vis,
                            ),
                        )
                    )

            p.close
            p.join

            for sv_thread in resultList:
                [date, hours, v, sv] = sv_thread.get()  # timeout=2000
                if date != None:
                    times.append(hours)
                    vis.append(v)
                    simvis.append(sv)
                    # print("%g %g %g" % (times[-1], vis[-1], simvis[-1]))
            p.terminate()
        except KeyboardInterrupt:
            print("control-c pressed")
            p.terminate()

    else:
        for fname in files:
            if "data.pkl" in fname:
                [date, hours, v, sv] = process_file(
                    dirname,
                    fname,
                    orientation_degrees,
                    spacing,
                    sample_duration,
                    simulate_vis,
                )

                times.append(hours)
                vis.append(v)
                simvis.append(sv)

                print("%g %s %s" % (times[-1], v.v, simvis))

    # vis = np.array(vis)
    simvis = np.array(simvis)
    return date, times, vis, simvis


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Simulate the Sky and calculate simulated visibilities."
    )
    parser.add_argument(
        "--data-dir", required=True, help="The filesystem path for the telescope data."
    )
    parser.add_argument(
        "--orientation-angle",
        required=True,
        type=float,
        default=-22.0,
        help="The orientation of the array (degrees).",
    )
    parser.add_argument(
        "--optimize",
        action="store_true",
        help="Optimize the correlation between the simulated and measured visibilities.",
    )
    parser.add_argument(
        "--spacing", type=float, default=0.5, help="The spacing between antennas."
    )
    parser.add_argument(
        "--sample-duration",
        type=float,
        default=1e-4,
        help="The length of time to simulate data for.",
    )
    args = parser.parse_args()
    dirname = args.data_dir
    sample_duration = args.sample_duration
    orientation_degrees = args.orientation_angle
    spacing = args.spacing

    if args.optimize:
        for theta in np.arange(
            orientation_degrees - 10.0, orientation_degrees + 10.0, 1.0
        ):
            for r in np.arange(spacing - 0.03, spacing + 0.03, 0.01):

                date, times, vis, simvis = get_correlation(
                    dirname, theta, r, sample_duration
                )
                corr = sum(vis * simvis)
                print("Correlation @%f %f %f" % (theta, r, corr))
    else:
        date, times, vis, simvis = get_correlation(
            dirname, orientation_degrees, spacing, sample_duration
        )
        corr = sum(vis * simvis)
        print("Correlation @%f %f %f" % (orientation_degrees, spacing, corr))

        plt.subplot(2, 1, 1)
        title = "Visibility Data from %s (th=%f, dx=%f)" % (
            str(date),
            orientation_degrees,
            spacing,
        )
        plt.title(title)
        plt.plot(times, vis, ".")
        plt.ylabel("Measured V")
        plt.xlabel("Hours since UTC ")
        plt.subplot(2, 1, 2)
        plt.plot(times, simvis, ".")
        plt.ylabel("Simulated V")
        plt.xlabel("Hours since UTC ")
        plt.savefig(
            os.path.join(
                dirname,
                "simulated_visibility_%f_%f.pdf" % (orientation_degrees, spacing),
            )
        )
        plt.show()
