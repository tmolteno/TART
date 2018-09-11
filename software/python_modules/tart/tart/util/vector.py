import numpy as np

from tart.util import angle

def unit_vector(vector):
    """ Returns the unit vector of the vector.    """
    return vector / np.linalg.norm(vector)

def angle_between(v1, v2):
    """ Returns the angle in radians between vectors 'v1' and 'v2'::

        >>> angle_between((1, 0, 0), (0, 1, 0))
        1.5707963267948966
        >>> angle_between((1, 0, 0), (1, 0, 0))
        0.0
        >>> angle_between((1, 0, 0), (-1, 0, 0))
        3.141592653589793
    """
    v1_u = unit_vector(v1)
    v2_u = unit_vector(v2)
    theta = np.arccos(np.dot(v1_u, v2_u))
    if np.isnan(theta):
        if (v1_u == v2_u).all():
            return angle.from_rad(0.0)
        else:
            return angle.from_rad(np.pi)
    return angle.from_rad(theta)
