# config.ru
# gem install rack-rpc
# gem install thin

$LOAD_PATH.unshift File.dirname(__FILE__)
#require 'restful_api'
#
#run ObjectServer
require 'RackServer'
run RackServer
