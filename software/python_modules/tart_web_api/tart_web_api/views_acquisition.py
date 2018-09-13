from flask import Flask
from flask import render_template, jsonify, send_file
from flask_jwt_extended import jwt_required

from tart_web_api.main import app

@app.route('/acquire/raw/save/<int:flag>', methods=['PUT'])
@jwt_required
def set_raw_save_flag(flag):
    """
    @api {put} /acquire/raw/save/<flag> Set save_flag for raw data acquisition.
    @apiGroup Acquisition
    @apiHeader (Authorization) {String} Authorization JWT authorization value.

    @apiName set_raw_save_flag
    @apiParam {Number =0-1} flag Default 0. To enable saving after acquistion set to 1.
    @apiSuccess {String} save Current save flag for raw mode.

    """
    runtime_config = app.config['CONFIG']
    r = runtime_config['raw']
    r['save'] = flag
    runtime_config['raw'] = r
    return jsonify({'save':runtime_config['raw']['save']})

@app.route('/acquire/vis/save/<int:flag>', methods=['PUT'])
@jwt_required
def set_vis_save_flag(flag):
    """
    @api {put} /acquire/vis/save/<flag> Set save_flag for visibility data acquisition.
    @apiGroup Acquisition
    @apiHeader (Authorization) {String} Authorization JWT authorization value.

    @apiName set_vis_save_flag
    @apiParam {Number =0-1} flag Default 0. To enable saving after acquistion set to 1.
    @apiSuccess {String} save Current save flag for visibility data acquisition.

    """
    runtime_config = app.config['CONFIG']
    r = runtime_config['vis']
    r['save'] = flag
    runtime_config['vis'] = r
    return jsonify({'save':runtime_config['vis']['save']})

@app.route('/acquire/raw/num_samples_exp/<int:exp>', methods=['PUT'])
@jwt_required
def set_raw_num_samples_exp(exp):
    """
    @api {put} /acquire/raw/num_samples_exp/<exp> Set exponent `exp` for number of samples for raw data acquisition (2**exp).
    @apiGroup Acquisition
    @apiHeader (Authorization) {String} Authorization JWT authorization value.

    @apiName set_raw_num_samples_exp
    @apiParam {Number = 16-24} exp Default 22.
    @apiSuccess {Number} exp Current exponent of number of samples.

    """
    runtime_config = app.config['CONFIG']
    if (exp >= 16) and (exp <= 24):
        r = runtime_config['raw']
        r['N_samples_exp'] = exp
        runtime_config['raw'] = r
    return jsonify({'N_samples_exp':runtime_config['raw']['N_samples_exp']})

@app.route('/acquire/vis/num_samples_exp/<int:exp>', methods=['PUT'])
@jwt_required
def set_vis_num_samples_exp(exp):
    """
    @api {put} /acquire/vis/num_samples_exp/<exp> Set exponent `exp` for number of samples for vis data acquisition (2**exp).
    @apiGroup Acquisition
    @apiHeader (Authorization) {String} Authorization JWT authorization value.

    @apiName set_vis_num_samples_exp
    @apiParam {Number = 16-24} exp Default 22.
    @apiSuccess {Number} exp Current exponent of number of samples.

    """
    runtime_config = app.config['CONFIG']
    if (exp >= 16) and (exp <= 24):
        r = runtime_config['vis']
        r['N_samples_exp'] = exp
        runtime_config['vis'] = r
    return jsonify({'N_samples_exp':runtime_config['vis']['N_samples_exp']})

@app.route('/acquire/raw/save', methods=['GET'])
def get_raw_save_flag():
    """
    @api {get} /acquire/raw/save Set save_flag for raw data acquisition.
    @apiGroup Acquisition

    @apiName get_raw_save_flag
    @apiSuccess {String} save Current save flag for raw mode.
    """
    runtime_config = app.config['CONFIG']
    return jsonify({'save':runtime_config['raw']['save']})

@app.route('/acquire/vis/save', methods=['GET'])
def get_vis_save_flag():
    """
    @api {get} /acquire/vis/save Set save_flag for visibility data acquisition.
    @apiGroup Acquisition

    @apiName get_vis_save_flag
    @apiSuccess {String} save Current save flag for visibility data acquisition.

    """
    runtime_config = app.config['CONFIG']
    return jsonify({'save':runtime_config['vis']['save']})

@app.route('/acquire/raw/num_samples_exp', methods=['GET'])
def get_raw_num_samples_exp():
    """
    @api {get} /acquire/raw/num_samples_exp Get exponent `exp` for number of samples for raw data acquisition (2**exp).
    @apiGroup Acquisition

    @apiName get_raw_num_samples_exp
    @apiSuccess {Number} exp Current exponent of number of samples.

    """
    runtime_config = app.config['CONFIG']
    return jsonify({'N_samples_exp':runtime_config['raw']['N_samples_exp']})

@app.route('/acquire/vis/num_samples_exp', methods=['GET'])
def get_vis_num_samples_exp():
    """
    @api {get} /acquire/vis/num_samples_exp Get exponent `exp` for number of samples for vis data acquisition (2**exp).
    @apiGroup Acquisition

    @apiName get_vis_num_samples_exp
    @apiSuccess {Number} exp Current exponent of number of samples.

    """
    runtime_config = app.config['CONFIG']
    return jsonify({'N_samples_exp':runtime_config['vis']['N_samples_exp']})
