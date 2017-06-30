build-prod:
	npm run ng build --prod --env=prod

upload: build-prod
	scp dist/* pi@tart2-raspberry:/var/www/html/
	scp dist/* pi@tart2-pear:/var/www/html/
