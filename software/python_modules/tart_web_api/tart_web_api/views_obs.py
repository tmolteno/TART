from flask import jsonify
from tart_web_api.main import app
import tart_web_api.database as db

@app.route('/raw/data')
def get_raw_data_file_handles():
    """
    @api {GET} /raw/data Get list of latest raw data files
    @apiName get_raw_data_file_handles
    @apiGroup Acquisiton

    @apiSuccess {Object}  body
    @apiSuccess {Number[]} body.filename Filename
    @apiSuccess {Number[]} body.checksum Checksum sha256
    """
    ret = db.get_raw_file_handle()
    for el in ret:
        el['filename'] = el['filename'][14:]
        el['timestamp'] = el['timestamp'][:-3]+'Z'
    return jsonify(ret)


@app.route('/vis/data')
def get_vis_data_file_handles():
    """
    @api {GET} /vis/data Get list of latest vis data files
    @apiName get_vis_data_file_handles
    @apiGroup Acquisiton

    @apiSuccess {Object}  body
    @apiSuccess {Number[]} body.filename Filename
    @apiSuccess {Number[]} body.checksum Checksum sha256
    """
    ret = db.get_vis_file_handle()
    for el in ret:
        el['filename'] = el['filename'][14:]
        el['timestamp'] = el['timestamp'][:-3]+'Z'
    return jsonify(ret)
