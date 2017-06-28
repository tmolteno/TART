from setuptools import setup

setup(
    name='telescope_api',
    packages=['telescope_api'],
    include_package_data=True,
    install_requires=[
        'flask',
	'tart_dsp'
    ],
)
