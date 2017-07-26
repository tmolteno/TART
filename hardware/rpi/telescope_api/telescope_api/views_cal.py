from flask import Flask, request
from flask import render_template, jsonify, send_file
from flask_jwt import jwt_required, current_identity

from telescope_api import app, get_config
import database as db

import datetime
from telescope_api import service
import multiprocessing

minimize_process = None

@jwt_required()
@app.route('/calibration/gain', methods=['POST',])
def set_gain():
  """
  @api {POST} /calibration/gain Set channel based complex gains.
  @apiName set_gain
  @apiGroup Calibration

  @apiHeader (Authorization) {String} Authorization JWT authorization value.

  @apiParam {Object}   body
  @apiParam {Number[]} body.gain List of channel gains
  @apiParam {Number[]} body.phase_offset List of channel phase offset
  """
  g = content['gain']
  ph = content['phase_offset']

  utc_date = datetime.datetime.utcnow()
  content = request.get_json(silent=False)
  g = content['gain']
  ph = content['phase_offset']
  db.insert_gain(utc_date, g, ph)
  return jsonify({})

@app.route('/calibration/gain', methods=['GET',])
def get_gain():
  """
  @api {GET} /calibration/gain Get channel based complex gains.
  @apiName get_gain
  @apiGroup Calibration

  @apiSuccess {Object}  body
  @apiSuccess {Number[]} body.gain List of channel gains
  @apiSuccess {Number[]} body.phase_offset List of channel phase offset
  """
  rows_dict =  db.get_gain()
  ret_gain = [rows_dict[i][2] for i in range(24)]
  ret_ph = [rows_dict[i][3] for i in range(24)]
  ret_dict = {"gain": ret_gain,\
    "phase_offset": ret_ph
  }
  return jsonify(ret_dict)

@jwt_required()
@app.route('/calibrate', methods=['POST',])
def post_calibration_from_vis():
  """
  @api {POST} /calibrate Start minimisation process by providing calibration measurements.
  @apiName post_calibration_from_vis
  @apiGroup Calibration

  @apiHeader (Authorization) {String} Authorization JWT authorization value.

  @apiParam {Object[]} body
  @apiParam {Number} body.el Elevation in decimal degree.
  @apiParam {Number} body.az Azimuth in decimal degree.
  @apiParam {Object[]} body.data Calibration data.
  @apiParam {Object[]} body.data.vis Visibilities from point source at specified elevation and azimuth.
  @apiParam {Object[]} body.data.timestamp Timestamp of measurement.
  @apiSuccess {String} status Status of optimisation process.
  """
  state = db.get_calibration_process_state()
  runtime_config = get_config()
  if state == 'idle':
    db.update_calibration_process_state('preparing')
    cal_measurements = request.get_json(silent=False)
    global minimize_process
    minimize_process = multiprocessing.Process(target=service.calibrate_from_vis, args=(cal_measurements, runtime_config))
    minimize_process.start()
    state = db.get_calibration_process_state()
  return jsonify({'status':state})

@app.route('/calibrate', methods=['GET',])
def get_calibrate_status():
  """
  @api {GET} /calibrate Get optimisation status.
  @apiName get_calibrate_status
  @apiGroup Calibration

  @apiSuccess {String} status Status of optimisation process.
  """
  state = db.get_calibration_process_state()
  return jsonify({'status':state})

