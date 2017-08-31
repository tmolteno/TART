from setuptools import setup

setup(
    name='tart_web_api',
    packages=['tart_web_api'],
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
