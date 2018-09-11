#
# Makefile for Creating and Installing package
# Tim Molteno tim@elec.ac.nz
#
# The lint target requires pylint, and python-pylint-flask
# to be installed.
#
.PHONY: doc, lint

develop: lint
	sudo python setup.py develop

lint:
	pylint --extension-pkg-whitelist=numpy --load-plugins pylint_flask tart_web_api

doc:
	pandoc --to=rst  README.md > README.txt
	
test_upload: doc
	sudo python setup.py sdist
	twine upload --repository testpypi dist/*

upload: doc
	sudo python setup.py sdist
	twine upload --repository pypi dist/*
