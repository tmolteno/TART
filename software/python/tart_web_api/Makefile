.PHONY: doc, lint

develop: lint
	sudo python setup.py develop

lint:
	pylint --load-plugins pylint_flask tart_web_api

doc:
	pandoc --to=rst  README.md > README.txt
	
test_upload: doc
	python setup.py sdist
	twine upload --repository testpypi dist/*

upload: doc
	python setup.py sdist
	twine upload --repository pypi dist/*
