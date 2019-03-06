$LOAD_PATH.unshift File.dirname(__FILE__)

require 'sinatra'
require 'sinatra/cors'
require 'thin'

require 'logger'
require 'BrdcHandler'
require 'GalileoHandler'

register Sinatra::Cors

set :allow_origin, "*"
set :allow_methods, "HEAD GET PUT DELETE OPTIONS"
set :allow_headers, "content-type"

class ObjectServer < Sinatra::Application

    def initialize()
        @brdc = BrdcHandler.new
        @gal = GalileoHandler.new
    end

#     options "*" do
#         response.headers["Allow"] = "HEAD,GET,PUT,DELETE,OPTIONS"
# 
#         # Needed for AngularJS
#         response.headers["Access-Control-Allow-Origin"] = "*"
#         response.headers["Access-Control-Allow-Headers"] = "X-Requested-With, X-HTTP-Method-Override, Content-Type, Cache-Control, Accept"
# 
#         halt HTTP_STATUS_OK
#     end
#     
#     configure do
#         enable :cross_origin
#     end
    
    before do
        if not params.has_key?('date')
            @request_date = Time.now.utc
        else
            date = params['date']
            @request_date = Time.parse(date.to_s).utc()
        end
    end

=begin
    @api {get} /position/ Request SV Positions in ECEF coordinates
    @apiName position
    @apiGroup Catalog

    @apiParam {String} [date=now] UTC date for the request

    @apiSuccess {List} ObjectList List of objects with coordinates in ECEF
    @apiSampleRequest /catalog/position
=end
    get '/position' do
        gps = @brdc.get_sv_positions(@request_date)
        gal = @gal.get_sv_positions(@request_date)
        return_message = gps + gal
        return_message.to_json
    end

=begin
    @api {get} /catalog/ Request Object Positions local horizontal (El Az) coordinates
    @apiName catalog
    @apiGroup Catalog

    @apiParam {String} [date=now] UTC date for the request
    @apiParam {Number} lat Latitude in decimal degrees of observer
    @apiParam {Number} lon Longitude in decimal degrees of observer
    @apiParam {Number} [alt=0.0] Altitude in meters of observer

    @apiSuccess {List} ObjectList List of objects with local horizontal (El Az) coordinates
    
    @apiSampleRequest /catalog/catalog?lat=-45.85&lon=170.54
=end
    get '/catalog' do
        return_message = {} 
        if not params.has_key?('lat')
            return_message[:status] = 'Error lat (latitude in decimal degrees) must be supplied'
            return return_message.to_json
        end
        if not params.has_key?('lon')
            return_message[:status] = 'Error lon (longitude in decimal degrees) must be supplied'
            return return_message.to_json
        end
        lat = params['lat'].to_f
        lon = params['lon'].to_f
        if not params.has_key?('alt')
            alt = 0.0
        else
            alt = params['alt'].to_f
        end
        
        gps = @brdc.get_sv_azel(@request_date, lat, lon, alt)
        gal = @gal.get_sv_azel(@request_date, lat, lon, alt)
        return_message = gps + gal
        return_message.to_json
    end
end
