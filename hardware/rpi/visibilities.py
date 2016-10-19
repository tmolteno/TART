#!/usr/bin/env python
from tartdsp import TartSPI
import numpy as np


if __name__ == '__main__':
    tart = TartSPI(speed=32000000)
    tart.reset(noisy=True)
    tart.debug(True, noisy=True)
    tart.start(noisy=True)
    for i in range(10):
        res = tart.vis_read(noisy=False)
        tart.status(noisy=True)
        print res
        print
    tart.close(noisy=True)
