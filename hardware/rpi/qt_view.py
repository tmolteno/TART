# -*- coding: utf-8 -*-
import PySide

from pyqtgraph.Qt import QtGui, QtCore
import numpy as np
import pyqtgraph as pg
import sys
import time
#from Queue import Queue
from multiprocessing import Queue
from threading import Thread

class QtPlotter(object):
    def __init__(self, app):
        super(QtPlotter, self).__init__()
        self.app = app
        self.create_QtPlotter()

    def create_QtPlotter(self):
        self.win = QtGui.QMainWindow()
        self.win.resize(400,400)
        self.page = QtGui.QWidget()
        self.graphicsView = pg.GraphicsView()
        vb = pg.ViewBox()
        self.graphicsView.setCentralItem(vb)
        vb.setAspectLocked()
        self.imv = pg.ImageItem()
        vb.addItem(self.imv)
        self.win.setCentralWidget(self.graphicsView)
        self.win.show()
        self.win.setWindowTitle('TART2 - Live View')
        self.timer = QtCore.QTimer()
        self.timer.timeout.connect(self.update)
        self.timer.start(0)
        self.q = Queue()
    
    #def worker(self, q)
    
    def getPort(self):
        return self.q

    def update(self):
        #try:
        print 'PlotQ size', self.q.qsize()
        #    if self.q.qsize()> 5:
        #        [self.q.get() for _ in range(5)]
        #        print 'dropping frames'
        #    else:
        data = self.q.get()
        self.imv.setImage(data,autoRange=True,autoLevels=True)
        self.app.processEvents()
        #except Queue.Empty:
        #    pass

def qtLoop():
    import sys
    if (sys.flags.interactive != 1) or not hasattr(QtCore, 'PYQT_VERSION'):
        QtGui.QApplication.instance().exec_()
        
## Start Qt event loop unless running in interactive mode or using pyside.
if __name__ == '__main__':
    app = QtGui.QApplication([])
    plotter = QtPlotter(app)
    q_handle = plotter.getPort()
    def producer():
        while True:
            q_handle.put(np.random.normal(size=(2**8, 2**8)).astype(np.float16))
            time.sleep(0.05)
    p = Thread(target=producer)
    p.daemon = True
    p.start()
    qtLoop()
