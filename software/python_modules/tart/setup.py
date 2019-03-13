from setuptools import setup, find_packages

with open('README.md') as f:
    readme = f.read()

setup(name='tart',
    version='1.1.0b3',
    description='Transient Array Radio Telescope Imaging and Operation Library',
    long_description=readme,
    long_description_content_type="text/markdown",
    url='http://github.com/tmolteno/TART',
    author='Tim Molteno',
    author_email='tim@elec.ac.nz',
    license='GPLv3',
    install_requires=['numpy', 'matplotlib', 'healpy', 'astropy', 'h5py'],
    packages=['tart', 'tart.imaging', 'tart.simulation', 'tart.operation', 'tart.util',\
            'tart.test',  'tart.imaging.test', 'tart.simulation.test', 'tart.operation.test', 'tart.util.test'],
    classifiers=[
        "Development Status :: 4 - Beta",
        "Topic :: Scientific/Engineering",
        "Topic :: Communications :: Ham Radio",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.7',
        "Intended Audience :: Science/Research"])
