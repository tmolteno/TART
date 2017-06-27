from flask import Flask

from flask import render_template, jsonify
from flask_jwt import jwt_required, current_identity

from telescope_api import app, get_config

@app.route('/', methods=['GET',])
def get_index():
    return render_template('index.html')

@app.route('/status/fpga', methods=['GET',])
def get_status_fpga():
    """
      @api {get} /status/fpga Request fpga information
      @apiName get_status_fpga
      @apiGroup status
      @apiParam {Number} id Users unique ID.
      @apiSuccess {String} firstname Firstname of the User.
      @apiSuccess {String} lastname  Lastname of the User.
    """
    runtime_config = get_config()
    if runtime_config.has_key("status"):
      return jsonify(runtime_config["status"])
    else:
      return jsonify({})

@app.route('/status/channel', methods=['GET',])
def get_status_channel_all():
    runtime_config = get_config()
    if runtime_config.has_key("channels"):
      return jsonify(runtime_config["channels"])
    else:
      return jsonify({})

@app.route('/status/channel/<int:channel_idx>', methods=['GET',])
def get_status_channel_n(channel_idx):
    runtime_config = get_config()
    if runtime_config.has_key("channels"):
      if ((channel_idx<24) and (channel_idx>-1)):
          return jsonify(runtime_config["channels"][channel_idx])
      else:
        return jsonify({})
    else:
      return jsonify({})

@app.route('/mode/', methods=['GET',])
def get_mode():
    runtime_config = get_config()
    return jsonify({'mode':runtime_config['mode']})

@app.route('/protected')
@jwt_required()
def protected():
    curr_id ='%s' % current_identity
    return jsonify({'current_identity' : curr_id, 'message': 'This is secret!!!'})

@app.route('/mode/diagnose', methods=['POST',])
def set_diagnose_mode():
    runtime_config = get_config()
    runtime_config['mode'] = 'diag'
    return 'Diagnose Mode'

@app.route('/mode/off', methods=['POST',])
def set_off_mode():
    runtime_config = get_config()
    runtime_config['mode'] = 'off'
    return 'Turned off'

@app.route('/mode/acquire/raw', methods=['POST',])
def set_acquire_raw_mode():
    runtime_config = get_config()
    runtime_config['mode'] = 'raw'
    return 'Raw Data acquisition'

@app.route('/mode/acquire/vis', methods=['POST',])
def set_acquire_vis_mode():
    runtime_config = get_config()
    runtime_config['mode'] = 'vis'
    return 'Visibility acquisition'

@app.route('/mode/calibrate', methods=['POST',])
def set_calibration_mode():
    runtime_config = get_config()
    runtime_config['mode'] = 'cal'
    return 'Calibration mode'
