# aptitude install python-pyside

import matplotlib

matplotlib.use('Qt4Agg')
matplotlib.rcParams['backend.qt4']='PySide'

from matplotlib.figure import Figure
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt4agg import NavigationToolbar2QTAgg as NavigationToolbar

from PySide import QtCore,QtGui

import sys
import numpy as np

class MatplotlibWidget(FigureCanvas):

    def __init__(self, parent=None,xlabel='x',ylabel='y',title='Title'):
        super(MatplotlibWidget, self).__init__(Figure())

        self.setParent(parent)
        self.figure = Figure()
        self.canvas = FigureCanvas(self.figure)
        self.axes = self.figure.add_subplot(111)

        self.axes.set_xlabel(xlabel)
        self.axes.set_ylabel(ylabel)
        self.axes.set_title(title)
        
    def plotDataPoints(self,x,y):        
        self.axes.clear()
        self.axes.plot(x,y,'bo-')
        self.draw()
     
class AppForm(QtGui.QMainWindow):
    def __init__(self, parent=None):
        QtGui.QMainWindow.__init__(self, parent)
        #self.x, self.y = self.get_data()
        self.create_main_frame()
        self.on_draw()

    def create_main_frame(self):
        self.main_frame = QtGui.QWidget()
        # create a matplotlib widget
        self.DataPlot = MatplotlibWidget(parent=self.main_frame)
        # create a layout inside the blank widget and add the matplotlib widget        
        layout = QtGui.QVBoxLayout(self.main_frame)        
        layout.addWidget(self.DataPlot,1)
        self.setCentralWidget(self.DataPlot)
 
    def on_draw(self):
        self.DataPlot.plotDataPoints(np.random.random(10),np.random.random(10))
    

def main():
    app = QtGui.QApplication(sys.argv)
    form = AppForm()
    form.show()
    app.exec_()

if __name__ == "__main__":
    main()