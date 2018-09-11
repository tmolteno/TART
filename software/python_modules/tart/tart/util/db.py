import sqlite3
try:
    import psycopg2
except Exception as e:
    pass

def db_connect(dbfile=None,\
    conn_parameter="host='<hostname>' dbname=tart2 user=<USER> password='<PASSWORD>'",\
    table="gps_signals"):
    if (dbfile != None):
        conn = sqlite3.connect(dbfile, timeout=60)
        paramstyle = sqlite3.paramstyle
    else:
        conn = psycopg2.connect(conn_parameter)
        paramstyle = psycopg2.paramstyle
    # Create table
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
