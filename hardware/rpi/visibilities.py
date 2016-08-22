#!/usr/bin/env python
from tartdsp import TartSPI
import numpy as np


if __name__ == '__main__':
    tart = TartSPI(speed=32000000)
    tart.reset(noisy=True)
    tart.debug(True, noisy=True)
    tart.start(noisy=True)
    tart.vis_read(noisy=True)
    tart.status(noisy=True)
    tart.close(noisy=True)
