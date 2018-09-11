import numpy as np

def unwrap(p):
    num_wrap = int(np.abs(p)/(2*np.pi))
    if p < 0:
        ret = p + (num_wrap+1)*2.*np.pi
    elif p >= 2.*np.pi:
        ret = p - num_wrap*2.*np.pi
    else:
        ret = p
    return ret

def compare_phases(p1, p2):
    up1 = unwrap(p1)
    up2 = unwrap(p2)
    diff = np.abs(up1 - up2)
    if diff > np.pi:
        ret = 2.*np.pi-diff
    else:
        ret = diff
    return ret

