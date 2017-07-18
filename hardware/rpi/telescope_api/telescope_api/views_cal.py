from flask import Flask, request
from flask import render_template, jsonify, send_file
from flask_jwt import jwt_required, current_identity

from telescope_api import app, get_config

import datetime
import sqlite3


def connect_to_db():
  try:
    con = sqlite3.connect('24_ant_setup/gains.db')
    c = con.cursor()
  except Exception as e:
    print type(e)     # the exception instance
    print e.args      # arguments stored in .args
    print e
  return con, c

def setup_db():
  con, c = connect_to_db()
  c.execute("CREATE TABLE IF NOT EXISTS calibration (date timestamp, antenna INTEGER, g_abs REAL, g_phase REAL, flagged BOOLEAN)")
  con.commit()
  con.close()

setup_db()

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
  c.execute('SELECT * FROM calibration ORDER BY date DESC LIMIT 24;')
  rows = c.fetchall()
  print rows
  rows_dict = {row[1]:row for row in rows}
  ret_gain = [rows_dict[i][2] for i in range(24)]
  ret_ph = [rows_dict[i][3] for i in range(24)]

  ret_dict = {"gain": ret_gain,\
    "phase_offset": ret_ph,\
    "flagged_baselines": []
  }
  return jsonify(ret_dict)
