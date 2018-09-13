import datetime

from flask import Flask, request
from flask import render_template, jsonify, send_file
from flask_jwt_extended import jwt_required

from tart_web_api.main import app
import tart_web_api.database as db

@jwt_required
@app.route('/channel/<int:channel_idx>/<int:enable>', methods=['PUT',])
def set_channel(channel_idx, enable):
    """
    @api {PUT} /channel/<channel_idx>/<enable> Enable/Disable channel manually.
    @apiName set_channel
    @apiGroup Calibration

    @apiHeader (Authorization) {String} Authorization JWT authorization value.

    @apiParam {Number={0..23}}  channel_idx Channel index
    @apiParam {Number={0,1}} enable Enabled: 1; Disabled: 0
    """
    db.update_manual_channel_status(channel_idx, enable)
    return jsonify({channel_idx:enable})

@app.route('/channel', methods=['GET',])
def get_all_channels():
    """
    @api {GET} /channel Get all channels and their enabled/disabled status.
    @apiName get_all_channels
    @apiGroup Calibration

    @apiSuccess {Object[]} response
    @apiSuccess {Number={{0..23}} response.channel_id Channel
    @apiSuccess {Number={0,1}} response.enabled Enabled: 1; Disabled: 0
    """
    ret = db.get_manual_channel_status()
    return jsonify(ret)
