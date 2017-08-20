apidoc:
	sed "s/localhost/`hostname`/g" -i apidoc.json
	apidoc -i ./ -o /var/www/html/doc
	sed "s/`hostname`/localhost/g" -i apidoc.json
public:
	apidoc -i ./ -o ./public_doc
	rsync -rvz public_doc/ tart@tart.elec.ac.nz:/var/www/html/doc
	#scp -r public_doc tart@tart.elec.ac.nz:/var/www/html/doc
