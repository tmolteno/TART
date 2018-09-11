# python setup.py develop
# from distutils.core import setup
import setuptools.command.test
from setuptools import setup, find_packages

with open('README.txt') as f:
    readme = f.read()

class TestCommand(setuptools.command.test.test):
    """ Setuptools test command explicitly using test discovery. """

    def _test_args(self):
        yield 'discover'
        for arg in super(TestCommand, self)._test_args():
            yield arg

setup(name='tart',
    version='0.14.8',
    description='Transient Array Radio Telescope Imaging and Operation Library',
    long_description=readme,
    url='http://github.com/tmolteno/TART',
    author='Tim Molteno',
    author_email='tim@elec.ac.nz',
    license='GPLv3',
    cmdclass={
        'test': TestCommand,
    },
    requires=['numpy', 'matplotlib', 'healpy', 'astropy'],
    packages=['tart', 'tart.imaging', 'tart.simulation', 'tart.operation', 'tart.util',\
            'tart.test',  'tart.imaging.test', 'tart.simulation.test', 'tart.operation.test', 'tart.util.test'],
    classifiers=[
        "Development Status :: 4 - Beta",
        "Topic :: Scientific/Engineering",
        "Topic :: Communications :: Ham Radio",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.7',
        "Intended Audience :: Science/Research"])

# aptitude install python-numpy python-matplotlib python-setuptools pkg-config python-scipy python-psycopg2 libfftw3-dev
# easy_install jsonrpclib pyfits healpy pyfftw json-->
#
