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
      @apiSuccess {String} hostname Hostname of the RPI
      @apiSuccess {String} timestamp UTC Timestamp
      @apiSuccess {Object} AQ_STREAM AQ_STREAM
          "data": 0
      @apiSuccess {Object} AQ_SYSTEM AQ_SYSTEM
          "512Mb": 1,
          "SDRAM ready": 1,
          "enabled": 1,
          "error": 1,
          "overflow": 1,
          "state": 7
      @apiSuccess {Object} SPI_STATS SPI_STATS
          "FIFO overflow": 0,
          "FIFO underrun": 0,
          "spi_busy": 0
      @apiSuccess {Object} SYS_STATS SYS_STATS
          "acq_en": 0,
          "cap_debug": 0,
          "cap_en": 0,
          "state": 0,
          "viz_en": 0,
          "viz_pend": 1
      @apiSuccess {Object} TC_CENTRE TC_CENTRE
        "centre": 1,
        "delay": 0,
        "drift": 0,
        "invert": 0
      @apiSuccess {Object} TC_DEBUG TC_DEBUG
        "count": 0,
        "debug": 0,
        "numantenna": 24,
        "shift": 0
      @apiSuccess {Object} TC_STATUS TC_STATUS
        "delta": 1,
        "phase": 2
      @apiSuccess {Object} TC_SYSTEM TC_SYSTEM
        "enabled": 1,
        "error": 1,
        "locked": 0,
        "source": 0
      @apiSuccess {Object} VX_DEBUG VX_DEBUG
        "limp": 0,
        "stuck": 0
      @apiSuccess {Object} VX_STATUS VX_STATUS
        "accessed": 0,
        "available": 0,
        "bank": 0,
        "overflow": 0
      @apiSuccess {Object} VX_STREAM VX_STREAM
        "data": 144
       @apiSuccess {Object} VX_SYSTEM VX_SYSTEM
        "blocksize": 0,
        "enabled": 0,
        "overwrite": 1
    """
    runtime_config = get_config()
    if runtime_config.has_key("status"):
      return jsonify(runtime_config["status"])
    else:
      return jsonify({})

@app.route('/status/channel', methods=['GET',])
def get_status_channel_all():
    """
    @api {get} /status/channel/ Request telescope all channel information.
    @apiGroup status

    @apiName get_status_channel_all
    @apiSuccess {Object[]} Array of channel information.
    """

    runtime_config = get_config()
    if runtime_config.has_key("channels"):
      return jsonify(runtime_config["channels"])
    else:
      return jsonify({})

@app.route('/status/channel/<int:channel_idx>', methods=['GET',])
def get_status_channel_i(channel_idx):
    """
    @api {get} /status/channel/:channel_idx Request specific channel information.
    @apiGroup status
    @apiParam {Number{0-23}} channel_idx Channel index

    @apiName get_status_channel_i
    @apiSuccess {Object} channel information of provided index.

    @apiSuccessExample {json} Success-Response:
      HTTP/1.1 200 OK
      {
        "id": 23,
        "phase": {
          "N_samples": 200,
          "measured": 3,
          "ok": 1,
          "stability": 1.0,
          "threshold": 0.95
        },
        "radio_mean": {
          "mean": 1.0,
          "ok": 0,
          "threshold": 0.2
        }
      }
    """
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
    """
    @api {get} /mode/ Request telescope operating mode
    @apiGroup mode

    @apiName get_mode
    @apiSuccess {String} mode Current mode of the telescope.
    """
    runtime_config = get_config()
    return jsonify({'mode':runtime_config['mode']})

@app.route('/protected')
@jwt_required()
def protected():
    curr_id ='%s' % current_identity
    return jsonify({'current_identity' : curr_id, 'message': 'This is secret!!!'})

@app.route('/mode/<mode>', methods=['POST',])
def set_mode(mode):
    """
    @api {post} /mode/<mode> Set telescope operating mode
    @apiGroup mode

    @apiName set_mode
    @apiParam {String ="off","diag","raw","vis","cal"} mode Telescope operation mode.
    @apiSuccess {String} mode Current mode of the telescope.

    """
    if mode in ['off','diag','raw','vis', 'cal']:
        runtime_config = get_config()
        runtime_config['mode'] = mode
    return jsonify({'mode':runtime_config['mode']})


