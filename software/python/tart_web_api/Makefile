.PHONY: doc

develop:
	sudo python setup.py develop

doc:
	pandoc --to=rst  README.md > README.txt
	
test_upload: doc
	python setup.py sdist
	twine upload --repository testpypi dist/*

upload: doc
	python setup.py sdist
	twine upload --repository pypi dist/*
