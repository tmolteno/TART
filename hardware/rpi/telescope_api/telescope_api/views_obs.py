from flask import Flask, request, send_from_directory
from flask import render_template, jsonify, send_file
from flask_jwt import jwt_required, current_identity

from telescope_api import app, get_config
import database as db

#from telescope_api import service
#import multiprocessing

#@app.route('/raw/<int:Id>')
#def download_obs(Id):
  #"""
  #@api {GET} /calibration/gain Get channel based complex gains.
  #@apiName get_gain
  #@apiGroup Calibration

  #@apiSuccess {Object}  body
  #@apiSuccess {Number[]} body.gain List of channel gains
  #@apiSuccess {Number[]} body.phase_offset List of channel phase offset
  #"""
  #resp = db.get_raw_file_handle()
  #print resp
  #for f in resp:
  #  if f['Id']==Id:
  #    fname = f['filename']
  #    print fname
  #return send_from_directory('/', fname, as_attachment=True)

@app.route('/raw/data')
def get_raw_data_file_handles():
  """
  @api {GET} /calibration/gain Get channel based complex gains.
  @apiName get_gain
  @apiGroup Calibration

  @apiSuccess {Object}  body
  @apiSuccess {Number[]} body.gain List of channel gains
  @apiSuccess {Number[]} body.phase_offset List of channel phase offset
  """
  ret = db.get_raw_file_handle()
  return jsonify(ret)
