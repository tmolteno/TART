#!/bin/bash
# needs spi library
#
# git clone https://github.com/lthiery/SPI-Py
# sudo python setup.py install

debug:
	python spartan6_spi_acquire.py --speed 8 --bramexp 10 --fast --debug --reset

acquire:
	python spartan6_spi_acquire.py --speed 8 --bramexp 20.94 --fast --reset

fulldebug:
	python spartan6_spi_acquire.py --speed 8 --bramexp 20.94 --fast --debug --reset

observation:
	python acquire.py --debug --data-directory test/
