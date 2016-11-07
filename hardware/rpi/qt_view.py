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
        exp = 8
        data = np.random.normal(size=(10,2**exp,2**exp))
        self.ports = []
        self.timer = pg.QtCore.QTimer()
        self.app = QtGui.QApplication([])
        self.win = pg.ImageView()
        self.win.setWindowTitle('TART2 - Live View')
        self.win.setImage(data[0])
        self.win.show()
        self.timer.timeout.connect(self.update)
        self.timer.start()

    def getPort(self):
        q = Queue.Queue()
        self.ports.append(q)
        return q

    def update(self):
        for q in self.ports:
            try:
                if q.qsize()> 10:
                    [q.get() for _ in range(10)] 
                print q.qsize()
                data = q.get()
                self.win.setImage(data)
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
            q_handle.put(np.random.random(size=(2**8, 2**8)))
            time.sleep(0.001)
    p = Thread(target=producer)
    p.daemon = True
    p.start()
    print 'here'
    qtLoop()
