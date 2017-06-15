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

from pyqtgraph.widgets.RawImageWidget import RawImageWidget
import time

class QtPlotter(object):
    def __init__(self, app):
        super(QtPlotter, self).__init__()
        self.app = app
        self.create_QtPlotter()
        self.data = None

    def create_QtPlotter(self):
        self.RAW = 1
        if self.RAW:
          self.win = QtGui.QMainWindow()
          self.win.resize(400,400)
          self.rawImg = RawImageWidget(self.win, scaled=True)
          self.win.setCentralWidget(self.rawImg)
          self.win.show()
        else:
          self.win = pg.GraphicsLayoutWidget()
          self.win.show()
          self.win.setWindowTitle('TART2 - Live View')
          view = self.win.addViewBox()
          view.setAspectLocked(True)
          self.img = pg.ImageItem(border='w')
          view.addItem(self.img)
        self.timer = QtCore.QTimer()
        self.timer.timeout.connect(self.update)
        self.timer.start(0)
        self.q = Queue()


    def getPort(self):
        return self.q

    def update(self):
        #qsize = self.q.qsize()
        #print 'PlotQ size',qsize
        #if qsize > 2:
            #[self.q.get() for _ in range(qsize-1)]
            #print '!!!!!!!!!!!!!!!!!!!!!! dropping frames when displaying  !!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        a = time.time()
        if not self.q.empty():
          #while not self.q.empty():
            #print self.q.qsize()
            #if self.data is None:
          self.data = self.q.get()
            #else:
              #self.data[:] = self.q.get()
              #print 'drop drop drop.'
          #print data[0]
          b = time.time()
          print b-a, 'getting data off queue'
          if self.RAW:
            d_max = self.data.max()
            self.data /= d_max
            self.data *= 255
            self.rawImg.setImage(self.data)
          else:
            self.data -= self.data.min()
            self.data /= self.data.max()
            self.data *= 255
            #self.img.setImage(data,autoRange=True,autoLevels=True)
            self.img.setImage(self.data)
          self.app.processEvents()
          c = time.time()
          print c-b, 'update done.'

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
