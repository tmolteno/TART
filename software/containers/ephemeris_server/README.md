# TART GPS Satellite Position Server

This is a server for both JSON-RPC and XML-RPC for GPS satellite positions. It exposes two methods

    get_sv_position(utc_date, sv) - return single SV positions
    get_sv_positions(utc_date) - return all SV positions

Both methods return the position in Earth Centered Earth Fixed Coordinates

## Installation

The server uses Rack and the thin webserver

    sudo npm install apidoc -g
    sudo aptitude instal gcc g++ make ruby-dev ruby-nokogiri
    sudo gem update
    sudo gem install thin rack-rpc jimson sinatra sinatra-cors nokogiri

Run the server with

    make

or 

    thin -C thin_config.yml -R config.ru

To stop the server type

    make stop
    
### Remote Install on EC2 Compute cloud

The remote server is on the AWS compute cloud, has DNS entry tart.elec.ac.nz

    rsync -avr --exclude 'tmp' --exclude 'log' ../object_position_server tart@tart.elec.ac.nz:.
    ssh tart@tart.elec.ac.nz
    cd object_position_server
    make stop
    make

You must change the thin_config.yml to use 0.0.0.0 as the listening port!


## Testing

    ruby BrdcClient.rb

### Python

    aptitude install python-bjsonrpc 
    python python_client.py
