# python setup.py develop
# from distutils.core import setup
from setuptools import setup, find_packages

with open('README.txt') as f:
    readme = f.read()

setup(name='tart_tools',
    version='0.1.1',
    description='Transient Array Radio Telescope Command Line Tools',
    long_description=readme,
    url='http://github.com/tmolteno/projects/TART',
    author='Tim Molteno',
    author_email='tim@elec.ac.nz',
    license='GPLv3',
    requires=['numpy', 'matplotlib', 'healpy', 'astropy', 'tart'],
    packages=['tart_tools'],
    scripts=['bin/tart_image', 'bin/tart_calibrate', 'bin/tart_calibration_data',
             'bin/tart_download_gains', 'bin/tart_upload_gains',
             'bin/tart_download_antenna_positions', 'bin/tart_upload_antenna_positions'],
    classifiers=[
        "Development Status :: 4 - Beta",
        "Topic :: Scientific/Engineering",
        "Topic :: Communications :: Ham Radio",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.7',
        "Intended Audience :: Science/Research"])
