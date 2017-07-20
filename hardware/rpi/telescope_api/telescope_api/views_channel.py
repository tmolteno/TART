from flask import Flask, request
from flask import render_template, jsonify, send_file
from flask_jwt import jwt_required, current_identity

from telescope_api import app, get_config, connect_to_db, get_manual_channel_status

import datetime

@jwt_required()
@app.route('/channel/<int:channel_idx>/<int:enable>', methods=['PUT',])
def set_channel(channel_idx,enable):
  con, c = connect_to_db()
  c.execute('UPDATE channels SET enabled = ? WHERE channel_id = ?', (enable,channel_idx))
  con.commit()
  return jsonify({channel_idx:enable})

@app.route('/channel/', methods=['GET',])
def get_all_channels():
  get_manual_channel_status()
  return jsonify(ret)

