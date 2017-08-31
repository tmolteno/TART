.PHONY: doc

develop:
	sudo python setup.py develop

doc:
	pandoc --to=rst  README.md > README.txt

dist: doc
	cp ../../FPGA/tart_spi/data/permute.txt tart_hardware_interface/permute.txt
	python setup.py sdist
	
test_upload: dist
	python setup.py sdist
	twine upload --repository testpypi dist/*

upload: dist
	python setup.py sdist
	twine upload --repository pypi dist/*
