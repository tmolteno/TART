apidoc:
	sed "s/localhost/`hostname`/g" -i apidoc.json
	apidoc -i ./ -o ./doc
	sed "s/`hostname`/localhost/g" -i apidoc.json
public:
	sed "s/localhost/tart.elec.ac.nz\/dev/g" -i apidoc.json
	sed "s/http/https/g" -i apidoc.json
	apidoc  -i ./ -o ./public_doc
	sed "s/tart.elec.ac.nz\/dev/localhost/g" -i apidoc.json
	sed "s/https/http/g" -i apidoc.json
	rsync -rvz public_doc/ tart@tart.elec.ac.nz:/var/www/html/doc
	#scp -r public_doc tart@tart.elec.ac.nz:/var/www/html/doc
