# -*- coding: utf-8 -*-
import PySide

from pyqtgraph.Qt import QtGui, QtCore
import numpy as np
import pyqtgraph as pg
import sys
import time
import Queue
from threading import Thread

class QtPlotter:
    def __init__(self):
        self.q = Queue.Queue()
        self.app = QtGui.QApplication([])
        self.win = pg.plot()
        self.win.setWindowTitle('TART2 - Live View')
        self.win.show()
        self.plot = [self.win.plot(pen=(i,24*1.3)) for i in range(24)]
        self.timer = pg.QtCore.QTimer()
        self.timer.timeout.connect(self.update)
        self.timer.start(0)

    def getPort(self):
        return self.q

    def update(self):
        try:
            print 'PlotQ size', self.q.qsize()
            if self.q.qsize()> 5:
                [self.q.get() for _ in range(5)]
                print 'dropping frames'
            else:
              data = self.q.get()
              for i in range(len(data)):
                self.plot[i].setData(data[i])
        except Queue.Empty:
            pass

def qtLoop():
    import sys
    if (sys.flags.interactive != 1) or not hasattr(QtCore, 'PYQT_VERSION'):
        QtGui.QApplication.instance().exec_()
        
## Start Qt event loop unless running in interactive mode or using pyside.
if __name__ == '__main__':
    plotter = QtPlotter()
    q_handle = plotter.getPort()
    def producer():
        while True:
            q_handle.put(np.asarray(np.random.random(size=(5,2**8)),dtype=np.float16))
            time.sleep(0.2)
    p = Thread(target=producer)
    p.daemon = True
    p.start()
    qtLoop()
