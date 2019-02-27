#
# Makefile for Creating and Installing package
# Tim Molteno tim@elec.ac.nz
#
# The lint target requires pylint, and python-pylint-flask
# to be installed.
#
.PHONY: lint

develop: lint
	sudo python3 setup.py develop

lint:
	pylint --extension-pkg-whitelist=numpy --load-plugins pylint_flask tart_web_api

test_upload:
	sudo python3 setup.py sdist
	twine upload --repository testpypi dist/*

upload:
	sudo python3 setup.py sdist
	twine upload --repository pypi dist/*
