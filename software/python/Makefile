.PHONY: doc

lint:
	pylint tart

install:
	sudo python setup.py install

develop:
	sudo python setup.py develop

doc:
	pandoc --to=rst  README.md > README.txt
	
test_upload:
	python setup.py sdist
	twine upload --repository testpypi dist/*

upload:
	pandoc --to=rst  README.md > README.txt
	python setup.py sdist
	twine upload --repository pypi dist/*
