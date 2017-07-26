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
  c.execute("CREATE TABLE IF NOT EXISTS sample_delay (date timestamp, delay REAL)")
  c.execute("CREATE TABLE IF NOT EXISTS calibration (date timestamp, antenna INTEGER, g_abs REAL, g_phase REAL, flagged BOOLEAN)")
  c.execute("CREATE TABLE IF NOT EXISTS channels (channel_id INTEGER, enabled BOOLEAN)")
  c.execute('SELECT * FROM channels;')
  con.commit()
  if len(c.fetchall())==0:
    ch = [(i,1) for i in range(24)]
    con.executemany("INSERT INTO channels(channel_id, enabled) values (?, ?)", ch)
  con.commit()
  con.close()

def get_manual_channel_status():
  con, c = connect_to_db()
  c.execute('SELECT * FROM channels;')
  rows = c.fetchall()
  ret = [{'channel_id':row[0],'enabled':row[1]} for row in rows]
  return ret

def update_manual_channel_status(channel_idx, enable):
  con, c = connect_to_db()
  c.execute('UPDATE channels SET enabled = ? WHERE channel_id = ?', (enable, channel_idx))
  con.commit()

def get_sample_delay():
  con, c = connect_to_db()
  c.execute('SELECT * FROM sample_delay ORDER BY datetime(date) DESC LIMIT 1')
  rows = c.fetchall()
  if len(rows) == 0:
    ret = 0
  else:
    ret = rows[0][1]
  return ret

def insert_sample_delay(timestamp, sample_delay):
  con, c = connect_to_db()
  SQL = "INSERT INTO sample_delay(date, delay) values (?, ?)"
  c.execute(SQL, (timestamp, sample_delay))
  con.commit()
  return 1

def insert_gain(utc_date, g, ph):
  con, c = connect_to_db()
  for ant_i in range(len(g)):
    c.execute("INSERT INTO calibration VALUES (?,?,?,?,?)", (utc_date, ant_i, g[ant_i],ph[ant_i],0) )
  con.commit()

def get_gain():
  rows_dict = {}
  con, c = connect_to_db()
  for i in range(24):
    c.execute('SELECT * FROM calibration WHERE antenna = '+str(i)+' ORDER BY date DESC LIMIT 1;')
    row = c.fetchall()[0]
    rows_dict[row[1]] = row
  return rows_dict
