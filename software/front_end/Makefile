
deploy:
	docker-compose build
	docker-compose down && docker-compose up -d
	rm -rf ./html # deleting old build files
	docker cp front_end_frontend_1:/usr/share/nginx/html .
	scp -r html tart@tart.elec.ac.nz:~/caddy/
	rm -rf ./html # deleting build files

