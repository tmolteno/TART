from setuptools import setup

setup(
    name='telescope_api',
    packages=['telescope_api'],
    include_package_data=True,
    install_requires=[
        'flask',
	'flask-jwt',
	'flask-jwt-extended',
	'flask-cli',
	'flask-cors',
	'flask-script',
	'tart_hardware_interface'
    ],
)
