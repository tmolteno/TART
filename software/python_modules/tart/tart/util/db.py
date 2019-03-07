'''
    Database Connection Utilities for TART
    
    Author: Tim Molteno 2014-2019 tim@elec.ac.nz
    Copyright GPLv3
'''

import sqlite3
try:
    import psycopg2
except Exception as e:
    pass

def db_connect(dbfile=None, table='gps_data'):
    if (dbfile != None):
        conn = sqlite3.connect(dbfile, timeout=60)
        paramstyle = sqlite3.paramstyle
    else:
        db_host= os.environ['POSTGRES_HOST']
        db_password = os.environ['POSTGRES_PASSWORD']
        db_user = os.environ['POSTGRES_USER']
        conn_parameter = "host='{}' dbname=tart2 user={} password='{}'".format(db_host, db_user, db_password)

        conn = psycopg2.connect(conn_parameter)
        paramstyle = psycopg2.paramstyle

    # Create table if necessary
    c = conn.cursor()
    c.execute("CREATE TABLE IF NOT EXISTS "+table+" (date timestamp, sv INTEGER, antenna INTEGER, el REAL, az REAL, correlation REAL, distance REAL)")
    conn.commit()
    def sql_(cmd, paramstyle):
        if paramstyle == 'qmark':
            ph = "?"
        elif paramstyle == 'pyformat':
            ph = "%s"
        else:
            raise Exception("Unexpected paramstyle: %s" % paramstyle)
        return cmd % {"ph": ph}
    sql = lambda cmd: sql_(cmd, paramstyle)
    return conn, sql
