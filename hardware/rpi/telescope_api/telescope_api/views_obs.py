from flask import jsonify
from telescope_api import app
import database as db

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
  return jsonify(ret)
