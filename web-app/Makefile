upload: deploy-raspberry deploy-pear deploy-orange
	echo 'done'

RSYNC_OPTS=-rvs --exclude=stats.json

deploy-raspberry:
	npm run raspberry-build
	rsync ${RSYNC_OPTS}  dist/* pi@tart2-raspberry:/var/www/html/

deploy-pear:
	npm run pear-build
	rsync ${RSYNC_OPTS} dist/* pi@tart2-pear:/var/www/html/

deploy-orange:
	npm run orange-build
	rsync ${RSYNC_OPTS}  dist/* pi@tart2-orange:/var/www/html/
