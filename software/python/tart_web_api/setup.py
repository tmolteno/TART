from setuptools import setup

with open('README.txt') as f:
    readme = f.read()


setup(
    name='tart_web_api',
    description='Transient Array Radio Telescope High-level HTTP Interface',
    long_description=readme,
    version='0.1.2',
    packages=['tart_web_api'],
    include_package_data=True,
    install_requires=[
        'flask',
        'flask-jwt-extended',
        'flask-cli',
        'flask-cors',
        'flask-script',
        'tart_hardware_interface',
        'tart'
    ],
    url='http://github.com/tmolteno/projects/TART',
    author='Tim Molteno, Max Scheel, Pat Suggate',
    author_email='tim@elec.ac.nz',
    license='GPLv3',
)



