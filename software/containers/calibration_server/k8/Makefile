KUBECTL=minikube kubectl --

build: 
	cd ..; ( eval $(minikube -p minikube docker-env) docker image build -t tart/cal . )

delete:
	${KUBECTL} delete -f tart-cal-deployment.yaml -f data-volume-persistentvolumeclaim.yaml

create:
	${KUBECTL} create -f tart-cal-deployment.yaml -f data-volume-persistentvolumeclaim.yaml
	${KUBECTL} get pods
	
cron:
	${KUBECTL} create -f cronjob.yaml
	${KUBECTL} get pods
	
	
test:
	( eval $(minikube -p minikube docker-env) ${KUBECTL} run -i --tty --rm debug --image=tart/cal --restart=Never --imagePullPolicy=Never -- )
