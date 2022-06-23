KUBECTL=minikube kubectl --

build: 
	echo "Container is built and published by github actions. https://github.com/tmolteno/tart_cal"

delete:
	-${KUBECTL} delete -f cronjob.yaml
	${KUBECTL} delete -f tart-cal-deployment.yaml -f data-volume-persistentvolumeclaim.yaml

create:
	${KUBECTL} create -f tart-cal-deployment.yaml -f data-volume-persistentvolumeclaim.yaml
	${KUBECTL} get pods
	
cron:
	-${KUBECTL} delete -f cronjob.yaml
	${KUBECTL} create -f cronjob.yaml
	${KUBECTL} get pods
	
	
test:
	${KUBECTL} run -i --tty --rm debug --image=ghcr.io/tmolteno/tart_cal:main --restart=Never
