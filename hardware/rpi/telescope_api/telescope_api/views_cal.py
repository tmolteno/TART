from flask import Flask, request
from flask import render_template, jsonify, send_file
from flask_jwt import jwt_required, current_identity

from telescope_api import app, get_config, connect_to_db

import datetime

@app.route('/calibration/gain', methods=['POST',])
def set_gain():
  con, c = connect_to_db()
  utc_date = datetime.datetime.utcnow()
  content = request.get_json(silent=False)
  g = content['gain']
  ph = content['phase_offset']
  fl = content['flagged_baselines']
  for ant_i in range(len(g)):
    c.execute("INSERT INTO calibration VALUES (?,?,?,?,?)", (utc_date, ant_i, g[ant_i],ph[ant_i],0) )
  con.commit()
  return '1'

@app.route('/calibration/gain', methods=['GET',])
def get_gain():
  con, c = connect_to_db()
  rows_dict = {}
  for i in range(24):
    c.execute('SELECT * FROM calibration WHERE antenna = '+str(i)+' ORDER BY date DESC LIMIT 1;')
    row = c.fetchall()[0]
    rows_dict[row[1]] = row
  
  ret_gain = [rows_dict[i][2] for i in range(24)]
  ret_ph = [rows_dict[i][3] for i in range(24)]

  ret_dict = {"gain": ret_gain,\
    "phase_offset": ret_ph,\
    "flagged_baselines": []
  }
  return jsonify(ret_dict)

