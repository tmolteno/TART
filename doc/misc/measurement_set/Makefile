MS=tart.ms
test:
	rm -rf ${MS}
	python3 json2ms.py --ms ${MS} --json data_2019_08_04_21_38_31_UTC.json

casa:
	docker-compose up -d
	docker-compose run --rm casa
