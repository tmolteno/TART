MS=tart.ms
test:
	rm -rf ${MS}
	python3 json2ms.py --ms ${MS} --json data_2019_08_04_21_38_31_UTC.json

install:
	sudo aptitude install python3-casacore
	sudo pip3 install dask-ms tart tart-tools

casa:
	xhost +
	docker-compose up -d
	docker-compose run --rm casa
