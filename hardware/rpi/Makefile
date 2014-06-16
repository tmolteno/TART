# Copyright (c) Tim Molteno, tim@elec.ac.nz. 2013.

all: prepare
	sudo python  interrupt_loop.py

monitor: prepare
	sudo python read_data.py --data-directory foo --monitor 1

noise: prepare
	sudo python read_data.py --data-directory foo --monitor 0

test:
	python testbench.py

prepare:
	sudo modprobe spi_bcm2708
