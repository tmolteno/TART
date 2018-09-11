lint:
	pylint --rcfile=../../coding_standards.pylint skymodel.py

solar:
	python solar_transit.py
png:
	python skymodel.py --n_sources 10 --png --show

test:
	pylint --rcfile=../../coding_standards.pylint antennas.py
	python testbench.py

