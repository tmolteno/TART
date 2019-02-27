from setuptools import setup, find_packages

with open('README.md') as f:
    readme = f.read()

setup(
    name='tart_web_api',
    description='Transient Array Radio Telescope High-level HTTP Interface',
    long_description=readme,
    long_description_content_type="text/markdown",
    version='0.2.0',
    packages=['tart_web_api'],
    include_package_data=True,
    install_requires=[
        'Flask',
        'flask-jwt-extended',
        'flask-cli',
        'flask-cors',
        'flask-script',
        'tart_hardware_interface',
        'tart'
    ],
    url='http://github.com/tmolteno/TART',
    author='Tim Molteno, Max Scheel, Pat Suggate',
    author_email='tim@elec.ac.nz',
    license='GPLv3',
)



