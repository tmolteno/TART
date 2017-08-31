# python setup.py develop
# from distutils.core import setup
from setuptools import setup, find_packages

with open('README.txt') as f:
    readme = f.read()

setup(name='tart',
    version='0.14.1',
    description='Transient Array Radio Telescope Imaging and Operation Library',
    long_description=readme,
    url='http://github.com/tmolteno/projects/TART',
    author='Tim Molteno',
    author_email='tim@elec.ac.nz',
    license='GPLv3',
    requires=['numpy'],
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
