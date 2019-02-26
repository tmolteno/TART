import sqlite3
import datetime

def connect_to_db():
    con = None
    c = None
    try:
        dbfile = 'tart_web_api_database.db'
        con = sqlite3.connect(dbfile)
        c = con.cursor()
    except Exception as e:
        print(type(e))     # the exception instance
        print(e.args)      # arguments stored in .args
        print(e)
    return con, c

def setup_db():
    con, c = connect_to_db()
    c.execute("CREATE TABLE IF NOT EXISTS raw_data (Id INTEGER PRIMARY KEY, date timestamp, filename TEXT, checksum TEXT)")
    c.execute("CREATE TABLE IF NOT EXISTS observation_cache_process (Id INTEGER PRIMARY KEY, date timestamp, state TEXT)")
    c.execute("CREATE TABLE IF NOT EXISTS vis_data (Id INTEGER PRIMARY KEY, date timestamp, filename TEXT, checksum TEXT)")
    c.execute("CREATE TABLE IF NOT EXISTS vis_cache_process (Id INTEGER PRIMARY KEY, date timestamp, state TEXT)")
    c.execute("CREATE TABLE IF NOT EXISTS calibration_process (Id INTEGER PRIMARY KEY, date timestamp, state TEXT)")
    c.execute("CREATE TABLE IF NOT EXISTS sample_delay (date timestamp, delay REAL)")
    c.execute("CREATE TABLE IF NOT EXISTS calibration (date timestamp, antenna INTEGER, g_abs REAL, g_phase REAL)")
    c.execute("CREATE TABLE IF NOT EXISTS channels (channel_id INTEGER, enabled BOOLEAN)")
    c.execute('SELECT * FROM channels;')
    con.commit()
    if len(c.fetchall()) == 0:
        ch = [(i, 1) for i in range(24)]
        con.executemany("INSERT INTO channels(channel_id, enabled) values (?, ?)", ch)
    con.commit()
    c.execute('SELECT * FROM calibration_process;')
    con.commit()
    if len(c.fetchall()) == 0:
        c.execute("INSERT INTO calibration_process(date, state) values (?, ?)", 
                  (datetime.datetime.utcnow(), 'idle'))
    con.commit()
    
    c.execute('SELECT * FROM calibration;')
    con.commit()
    if len(c.fetchall()) == 0:
        utc_date = datetime.datetime.utcnow()
        g = [1,]*24
        ph = [0,]*24
        insert_gain(utc_date, g, ph)
    con.close()

def get_manual_channel_status():
    con, c = connect_to_db()
    c.execute('SELECT * FROM channels;')
    rows = c.fetchall()
    ret = [{'channel_id':row[0], 'enabled':row[1]} for row in rows]
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

##################
#  Antenna Gain  #
##################

def insert_gain(utc_date, g, ph):
    con, c = connect_to_db()
    for ant_i in range(len(g)):
        c.execute("INSERT INTO calibration VALUES (?,?,?,?)", 
                  (utc_date, ant_i, g[ant_i], ph[ant_i]))
    con.commit()

def get_gain():
    rows_dict = {}
    con, c = connect_to_db()

    c.execute('SELECT date, antenna, g_abs, g_phase from calibration WHERE date = (SELECT MAX(date) FROM calibration) ORDER BY antenna;')
    rows = c.fetchall()
    for row in rows:
        rows_dict[row[1]] = row

    if False:
        for i in range(24):
            c.execute('SELECT * FROM calibration WHERE antenna = '+str(i)+' ORDER BY date DESC LIMIT 1;')
            row = c.fetchall()[0]
            rows_dict[row[1]] = row
    return rows_dict

#########################
#  Calibration Process  #
#########################

def update_calibration_process_state(state):
    con, c = connect_to_db()
    c.execute('UPDATE calibration_process SET state = ? WHERE Id = ?', (state, 1))
    con.commit()

def get_calibration_process_state():
    con, c = connect_to_db()
    c.execute('SELECT * FROM calibration_process')
    rows = c.fetchall()
    if len(rows) == 0:
        ret = 'Error'
    else:
        ret = rows[0][2]
    return ret

####################
#  Raw Data Cache  #
####################

def insert_raw_file_handle(filename, checksum):
    con, c = connect_to_db()
    timestamp = datetime.datetime.utcnow()
    c.execute("INSERT INTO raw_data(date, filename, checksum) VALUES (?,?,?)",
              (timestamp, filename, checksum))
    con.commit()

def remove_raw_file_handle_by_Id(Id):
    con, c = connect_to_db()
    c.execute("DELETE FROM raw_data WHERE Id=?", (Id,))
    con.commit()

def get_raw_file_handle():
    con, c = connect_to_db()
    c.execute("SELECT * FROM raw_data ORDER BY date DESC")
    rows = c.fetchall()
    ret = [{'filename':row[2], 'timestamp':row[1], 'checksum':row[3], 'Id':row[0]} for row in rows]
    return ret

def update_observation_cache_process_state(state):
    con, c = connect_to_db()
    ts = datetime.datetime.utcnow()
    c.execute('UPDATE observation_cache_process SET state = ?, date = ? WHERE Id = ?', (state, ts, 1))
    con.commit()

def get_observation_cache_process_state():
    con, c = connect_to_db()
    c.execute('SELECT * FROM observation_cache_process')
    rows = c.fetchall()
    if len(rows) == 0:
        ret = 'Error'
    else:
        ret = {'state':rows[0][2], 'timestamp':rows[0][1]}
    return ret

###########################
#  Visibility Data Cache  #
###########################

def insert_vis_file_handle(filename, checksum):
    con, c = connect_to_db()
    timestamp = datetime.datetime.utcnow()
    c.execute("INSERT INTO vis_data(date, filename, checksum) VALUES (?,?,?)", 
              (timestamp, filename, checksum))
    con.commit()

def remove_vis_file_handle_by_Id(Id):
    con, c = connect_to_db()
    c.execute("DELETE FROM vis_data WHERE Id=?", (Id,))
    con.commit()

def get_vis_file_handle():
    con, c = connect_to_db()
    c.execute("SELECT * FROM vis_data ORDER BY date DESC")
    rows = c.fetchall()
    ret = [{'filename':row[2], 'timestamp':row[1], 'checksum':row[3], 'Id':row[0]} for row in rows]
    return ret

def update_vis_cache_process_state(state):
    con, c = connect_to_db()
    ts = datetime.datetime.utcnow()
    c.execute('UPDATE vis_cache_process SET state = ?, date = ? WHERE Id = ?', (state, ts, 1))
    con.commit()

def get_vis_cache_process_state():
    con, c = connect_to_db()
    c.execute('SELECT * FROM vis_cache_process')
    rows = c.fetchall()
    if len(rows) == 0:
        ret = 'Error'
    else:
        ret = {'state':rows[0][2], 'timestamp':rows[0][1]}
    return ret
