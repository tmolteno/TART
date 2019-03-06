# Test a whole group of fix files

# 
reload:
	thin -C thin_config.yml -R config.ru restart

server:
	thin -C thin_config.yml -R config.ru start

stop:
	thin -C thin_config.yml stop

# 1:47 seconds XMLRPC Webrick
# 1:00 XMLRPC thin
# 0:28 JSONRPC (jimson)
test:
	time ruby BrdcClient.rb 

local:
	time ruby -I . TestClient.rb 

doc:
	apidoc -i ./ -o ./public_doc

install: doc
	rsync -avr --exclude 'tmp' --exclude 'log' ../object_position_server tart@tart.elec.ac.nz:.
