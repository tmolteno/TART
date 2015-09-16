from PyNEC import *
import numpy as np
import math

def inverted_v(freq, base, length, angle):
    #creation of a nec context
    context=nec_context()

    #get the associated geometry
    geo = context.get_geometry()


    conductivity = 1.45e7 # Copper
    ground_conductivity = 0.002
    ground_dielectric = 10

    dx = length*math.cos(angle)
    dy = length*math.cos(angle)
    
    dz = length*math.sin(angle)
    
    wavelength = 3e8/(1e6*freq)
    n_seg = int(math.ceil(90*length/wavelength))

    p0 = [0.005, 0.0, base]
    p1 = [-0.005, 0.0, base]
    geo.wire(1, n_seg, p0[0], p0[1], p0[2], dx, dy, base-dz, 0.001, 1.0, 1.0)
    geo.wire(2, n_seg, p0[0], p0[1], p0[2], dx, -dy, base-dz, 0.001, 1.0, 1.0)

    geo.wire(3, n_seg, p1[0], p1[1], p1[2], -dx, dy, base-dz, 0.001, 1.0, 1.0)
    geo.wire(4, n_seg, p1[0], p1[1], p1[2], -dx, -dy, base-dz, 0.001, 1.0, 1.0)

    geo.wire(5, 3, p1[0], p1[1], p1[2], p0[0], p0[1], p0[2], 0.001, 1.0, 1.0)
    

    context.geometry_complete(0)
    #context.gn_card(0,16,ground_dielectric,ground_conductivity,length,0.001,0,0)
    context.ld_card(5, 0, 0, 0, conductivity, 0.0, 0.0)
    context.ex_card(0, 5, 2, 0, 0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0)
    context.fr_card(0, 1, freq, 0)

    n_theta = 90
    n_phi = 5
    theta0 = 0.0
    phi0 = 45.0
    delta_theta = int(360/n_theta)
    delta_phi = int(45/n_phi)

    #add a "rp" card to specify radiation pattern sampling parameters and to cause program execution
    context.rp_card(0, n_theta, n_phi, 1, 5, 0, 1, theta0, phi0, delta_theta, delta_phi, 1.0, 0.0)

    ip = context.get_input_parameters(0)
    z = ip.get_impedance()

    #get the radiation_pattern
    rp = context.get_radiation_pattern(0)

    # Gains are in decibels
    gains_db = rp.get_gain()
    #gains = 10.0**(gains_db / 10.0)
    thetas = rp.get_theta_angles() * 3.1415 / 180.0
    phis = rp.get_phi_angles() * 3.1415 / 180.0
    return z, gains_db, thetas, phis

# Plot stuff
import matplotlib.pyplot as plt
from antenna_util import reflection_coefficient

z, gains_db, thetas, phis = inverted_v(freq=1545.42, base=0.1, length=0.130, angle=math.pi/6)

print z
print reflection_coefficient(z,50)

ax = plt.subplot(111, polar=True)
for j in range(0,5):
  ax.plot(thetas, gains_db[:,j] + 20, linewidth=3, label=("%s" % phis[j]))
ax.grid(True)

ax.set_title("Gain at an elevation of 45 degrees", va='bottom')
plt.savefig('RadiationPattern.png')
plt.show()
