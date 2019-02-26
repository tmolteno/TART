import requests
import json

import numpy as np
import matplotlib.pyplot as plt

import time
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import argparse

def update(frame_number):    
    #r = requests.get('https://tart.elec.ac.nz/lab/api/v1/imaging/vis')
    r = requests.get('{}/api/v1/imaging/vis'.format(args.api))
    r_d = json.loads(r.text)

    for v in r_d['data']:
        if (v['i'] == i) and (v['j'] == j):
            v_comp = complex(v['re'],v['im'])
            vis_ij.append(v_comp)

    data['t'] = np.arange(len(vis_ij))
    data['abs'] = np.abs(vis_ij)
    data['ph'] = np.angle(vis_ij)
    #print data['abs']
    # Update the scatter collection with the new positions.
    scat1.set_offsets(zip(data['t'],data['abs']))
    scat2.set_offsets(zip(data['t'],data['ph']))
    ax.set_xlim(min(data['t'][-args.num:]),max(data['t']))
    ax2.set_xlim(min(data['t'][-args.num:]),max(data['t']))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Monitor TART visibilities.')
    parser.add_argument('--i', type=int, default=0, help='i')
    parser.add_argument('--j', type=int, default=1, help='j')
    parser.add_argument('--interval', type=int, default=500, help='Interval in ms, between vis requests')
    parser.add_argument('--num', type=int, default=500, help='Number of consecutive datapoints being plotted in time')
    parser.add_argument('--api', type=str, default='https://tart.elec.ac.nz/signal', help='Telescope API URL')
    
    args = parser.parse_args()
    
    fig = plt.figure(figsize=(7, 7))
    ax = fig.add_subplot(211)
    ax.set_title("Antenna Pair: {}-{}".format(args.i,args.j))
    ax2 = fig.add_subplot(212)

    ax.set_xlim(-1, 1)
    ax.set_ylim(0.01, 1)
    ax.set_yscale("log", nonposy='clip')
    ax.grid(True, which="both")

    ax2.set_xlim(-1, 1)
    ax2.set_ylim(-np.pi, np.pi)
    ax.set_ylabel('Magnitude [1]')
    ax2.set_ylabel('Phase [rad]')
    ax2.grid(True, which="both")

    vis_ij = []

    # Construct the scatter which we will update during animation
    data = {'t': [],'abs':[],'ph':[]}
    scat1 =  ax.scatter(data['t'],data['abs'], color='green',s=2)
    scat2 = ax2.scatter(data['t'],data['ph'], color='red',s=2)

    # index i,j start from 0
    i = args.i
    j = args.j

    animation = FuncAnimation(fig, update, interval=args.interval)
    plt.tight_layout()
    plt.show()
