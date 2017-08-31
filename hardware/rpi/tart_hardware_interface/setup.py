from setuptools import setup

setup(
    name='tart_hardware_interface',
    packages=['tart_hardware_interface'],
    install_requires=[
        'spidev',
	'numpy',
    ],
)
