"""Copyright (C) Max Scheel 2016. All rights reserved"""
import numpy as np
from multiprocessing import Pool
from tart.simulation import antennas
import time

try:
    import cPickle as pickle
except:
    import pickle


def get_vis_parallel(sky, cor, rad, ants, ant_models, config, time, mode="simp"):
    p = Pool()
    resultList = []
    ret_vis_list = []
    try:
        if type(sky) in [np.ndarray, list]:
            for s in sky:
                resultList.append(
                    p.apply_async(
                        get_vis, (s, cor, rad, ants, ant_models, config, time, mode)
                    )
                )
        if type(time) in [np.ndarray, list]:
            for t in time:
                resultList.append(
                    p.apply_async(
                        get_vis, (sky, cor, rad, ants, ant_models, config, t, mode)
                    )
                )
        p.close
        p.join
        for thread in resultList:
            vis = thread.get()
            if vis != None:
                ret_vis_list.append(vis)
        p.terminate()
    except KeyboardInterrupt:
        print("control-c pressed")
        p.terminate()
    return ret_vis_list


def get_vis(
    sky,
    cor,
    rad,
    ants,
    ant_models,
    config,
    timestamp,
    mode="simp",
    seed=None,
):
    np.random.seed(seed=seed)
    sources = sky.gen_photons_per_src(timestamp, radio=rad, config=config, n_samp=1)
    # sources = sky.gen_n_photons(config, timestamp, radio=rad, n=10)
    # print('debug: total flux',  np.array([src.jansky(timestamp) for src in self.known_objects]).sum())
    # print('debug: total amplitude', np.array([src.amplitude for src in sources]).sum())

    if mode == "full":
        timebase = np.arange(0, rad.sample_duration, 1.0 / rad.sampling_rate)
        ant_sigs_full = antennas.antennas_signal(ants, ant_models, sources, timebase)
        obs = rad.get_full_obs(ant_sigs_full, timestamp, config, timebase)
        v = cor.correlate_roll(obs)

    elif mode == "mini":
        v = antennas.antennas_simp_vis(
            ants, ant_models, sources, timestamp, config, rad.noise_level
        )
    else:
        ant_sigs_simp = antennas.antennas_simplified_signal(
            ants, ant_models, sources, rad.baseband_timebase, rad.int_freq, seed=seed
        )
        obs = rad.get_simplified_obs(ant_sigs_simp, timestamp, config=config, seed=seed)
        v = cor.correlate_roll(obs)
    return v


def get_vis_parallel_segmented(
    sky, cor, rad, ants, ant_models, config, time, mode="simp", segment_dir="temp/"
):
    vis_per_segment = 1000
    num_segments = int(np.ceil(len(sky) / float(vis_per_segment)))

    for i in range(num_segments):
        print("computing segment %i of %i" % (i, num_segments))
        start_idx = i * vis_per_segment
        end_idx = (i + 1) * vis_per_segment
        vis_seg = get_vis_parallel(
            sky[start_idx:end_idx],
            cor,
            rad,
            ants,
            ant_models,
            config,
            time,
            mode="simp",
        )
        pickle.dump(vis_seg, open(segment_dir + "segment_%010i.pkl" % i, "wb"))

    # //print('now combining segments.')
    ret = []
    # print(num_segments)
    # for i in range(num_segments):
    # print(i)
    # vis_seg = pickle.load( open( segment_dir+'segment_%010i.pkl' %i, "rb" ) )
    # ret += vis_seg
    return ret
