upload: deploy-raspberry deploy-pear
	echo 'done'
    

deploy-raspberry:
	npm run raspberry-build
	scp dist/* pi@tart2-raspberry:/var/www/html/

deploy-pear:
	npm run pear-build
	scp dist/* pi@tart2-pear:/var/www/html/
