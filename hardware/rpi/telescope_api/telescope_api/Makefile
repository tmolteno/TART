apidoc:
	sed "s/localhost/`hostname`/g" -i apidoc.json
	apidoc -i ./ -o /var/www/html/doc
	sed "s/`hostname`/localhost/g" -i apidoc.json
