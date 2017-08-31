

lint:
	pylint tart

install:
	sudo python setup.py install

develop:
	sudo python setup.py develop

# Upload to pypi. See 
upload:
	pandoc --to=rst  README.md > README.txt
# 	python setup.py sdist upload -r pypitest
	python setup.py sdist
	twine upload --repository testpypi dist/*
