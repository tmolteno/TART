.PHONY: doc

develop: doc
	sudo python setup.py develop

lint:
	pylint tart

test:
	python3 setup.py test

doc:
	pandoc --to=rst  README.md > README.txt
	
test_upload: doc
	python setup.py sdist
	twine upload --repository testpypi dist/*

upload: doc
	python setup.py sdist
	twine upload --repository pypi dist/*
