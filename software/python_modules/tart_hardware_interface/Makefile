.PHONY: doc

develop:
	sudo python setup.py develop

doc:
	pandoc --to=rst  README.md > README.txt

dist: doc
	cp ../../../hardware/FPGA/tart_spi/data/permute.txt tart_hardware_interface/permute.txt
	python3 setup.py sdist
	
test_upload: dist
	twine upload --repository testpypi dist/*

upload: dist
	twine upload --repository pypi dist/*
