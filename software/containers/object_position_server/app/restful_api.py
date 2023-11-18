# Object Position Server Main API
#
# Author Tim Molteno tim@elec.ac.nz (c) 2013-2018


from flask import Flask
from flask import jsonify, request
from flask_cors import CORS, cross_origin

import tart.util.utc as utc
from tart.util import angle
import traceback

import norad_cache
from dateutil import parser
import sun_object

waas_cache = norad_cache.NORADCache()
# extra_cache = norad_cache.ExtraCache()
gps_cache = norad_cache.GPSCache()
galileo_cache = norad_cache.GalileoCache()
beidou_cache = norad_cache.BeidouCache()

sun = sun_object.SunObject()


def parse_date(request):
    if 'date' in request.args:
        try:
            date_string = request.args.get('date')
            
            if date_string=="now":
                d = utc.now()
            else:
                # Deal with a URL that has a + sign replaced by a space
                dt = parser.parse(date_string.replace(' ', '+'))
                d = utc.to_utc(dt)
                # print(f"Parsing date: {date_string} -> {dt} -> {d}")
        except Exception as err:
            raise Exception("Invalid Date '{}' {}".format(date_string, err))
    else:
        d = utc.now()

    current_date = utc.now()
    if ((d - current_date).total_seconds() > 86400.0):
        raise Exception(
            f"Date '{date_string}' more than 24 hours in future. {current_date} {d}")

    return d


def get_required_parameter(request, param_name):
    if param_name in request.args:
        return request.args.get(param_name)
    else:
        # app.logger.error("Missing Required Parameter {}".format(param_name))
        raise Exception("Missing Required Parameter '{}'".format(param_name))


# sudo pip install Flask
app = Flask(__name__)
CORS(app)

if not app.debug:
    import logging
    from logging.handlers import RotatingFileHandler
    log_handler = RotatingFileHandler(
        "catalog.log", mode='a', maxBytes=100000, backupCount=5, encoding=None, delay=False)
    log_handler.setLevel(logging.WARNING)
    app.logger.addHandler(log_handler)


@app.errorhandler(Exception)
def handle_exception(e):
    app.logger.error(e)
    tb = traceback.format_exc()
    app.logger.error(tb)
    return "Exception: {}".format(e)


"""
    @api {get} /catalog/ Request Object Positions local horizontal (El Az) coordinates
    @apiName catalog
    @apiGroup Catalog

    @apiParam {String} [date=now] UTC date for the request
    @apiParam {Number} lat Latitude in decimal degrees of observer
    @apiParam {Number} lon Longitude in decimal degrees of observer
    @apiParam {Number} [elevation=0.0] Ignore objects below the specified elevation in decimal degrees
    @apiParam {Number} [alt=0.0] Altitude in meters of observer

    @apiSuccess {List} ObjectList List of objects with local horizontal (El Az) coordinates

    @apiSampleRequest /catalog?lat=-45.85&lon=170.54
"""


@app.route('/catalog', methods=['GET', ])
def get_catalog():
    date = parse_date(request)
    lat = angle.from_dms(float(get_required_parameter(request, 'lat')))
    lon = angle.from_dms(float(get_required_parameter(request, 'lon')))
    try:
        elevation = float(request.args.get('elevation'))
    except Exception:
        elevation = 0.0

    alt = 0.0
    ret = waas_cache.get_az_el(date, lat, lon, alt, elevation)
    ret += gps_cache.get_az_el(date, lat, lon, alt, elevation)
    ret += galileo_cache.get_az_el(date, lat, lon, alt, elevation)
    ret += beidou_cache.get_az_el(date, lat, lon, alt, elevation)
    # ret += extra_cache.get_az_el(date, lat, lon, alt, elevation)
    ret += sun.get_az_el(date, lat, lon, alt, elevation)
    return jsonify(ret)


"""
    @api {get} /position Request SV Positions in ECEF coordinates
    @apiName position
    @apiGroup Catalog

    @apiParam {String} [date=now] UTC date for the request

    @apiSuccess {List} ObjectList List of objects with coordinates in ECEF
    @apiSampleRequest /position
"""


@app.route('/position', methods=['GET', ])
def get_pos():
    try:
        date = parse_date(request)
        ret = waas_cache.get_positions(date)
        ret += gps_cache.get_positions(date)
        # ret += extra_cache.get_positions(date)
        ret += galileo_cache.get_positions(date)
        ret += beidou_cache.get_positions(date)
        return jsonify(ret)
    except Exception as err:
        tb = traceback.format_exc()
        ret = "Exception: {}".format(err)
        lines = tb.split("\n")
        return jsonify({"error": ret, "traceback": lines})


if __name__ == '__main__':
    print("Hello world")
    app.run(port=8876, host='0.0.0.0')
