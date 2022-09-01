KUBECTL=minikube kubectl --

build: 
	echo "Container is built and published by github actions. https://github.com/tmolteno/tart_cal"

delete:
	-${KUBECTL} delete -f signal_cal_cronjob.yaml -f rhodes_cal_cronjob.yaml
	${KUBECTL} delete -f data-volume-persistentvolumeclaim.yaml

create:
# 	${KUBECTL} create -f tart-cal-deployment.yaml -f data-volume-persistentvolumeclaim.yaml
	${KUBECTL} create -f data-volume-persistentvolumeclaim.yaml
	${KUBECTL} get pods
	
cron:
	-${KUBECTL} delete -f signal_cal_cronjob.yaml -f rhodes_cal_cronjob.yaml
	${KUBECTL} create -f signal_cal_cronjob.yaml -f rhodes_cal_cronjob.yaml
	${KUBECTL} get pods
	
	
test:
	${KUBECTL} run -i --tty --rm debug --image=ghcr.io/tmolteno/tart_cal:main --restart=Never
