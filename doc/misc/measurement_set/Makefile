MS=tart.ms
test:
	rm -rf ${MS}
	python3 json2ms.py --ms ${MS} --json data_2019_08_04_21_38_31_UTC.json

NPIX=720
PIXEL_SIZE_IN_DEG=0.25
clean:
	wsclean -name IMAGENAME -size ${NPIX} ${NPIX} -scale ${PIXEL_SIZE_IN_DEG} -niter 0 ${MS}

tar:
	tar -czvf tart.ms.tar.gz tart.ms 

install:
	sudo aptitude install python3-casacore wsclean
	sudo pip3 install dask-ms tart tart-tools

casa:
	xhost +
	docker-compose up -d
	docker-compose run --rm casa
