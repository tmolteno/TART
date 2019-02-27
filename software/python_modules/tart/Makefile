
develop:
	sudo python setup.py develop

lint:
	pylint tart

test:
	python3 setup.py test --test-suite=tart.imaging.test.test_visibility.TestVisibility

test_upload:
	python setup.py sdist
	twine upload --repository testpypi dist/*

upload:
	python setup.py sdist
	twine upload --repository pypi dist/*
