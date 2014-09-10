# python setup.py develop
# from distutils.core import setup
from setuptools import setup, find_packages

setup(name='tart',
      version='0.14',
      description='Transient Array Radio Telescope Imaging and Operation Library',
      url='http://github.com/tmolteno/projects/TART',
      author='Tim Molteno',
      author_email='tim@elec.ac.nz',
      requires=['numpy'],
      license='GPLv3',
      packages=['tart', 'tart.imaging', 'tart.simulation', 'tart.operation', 'tart.util',\
       'tart.test',  'tart.imaging.test', 'tart.simulation.test', 'tart.operation.test', 'tart.util.test'],
)
