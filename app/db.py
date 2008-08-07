import MySQLdb

from shared import config


try:
    db = MySQLdb.connect(
        host=config['MySQL']['address']['host'],
        port=config['MySQL']['address']['port'],
        user=config['MySQL']['username'],
        passwd=config['MySQL']['password'],
        db=config['MySQL']['database'],
    )
except MySQLdb.OperationalError, msg:
    if msg[0] == 1045: # Access denied
        print 'Cannot connect to database - %s' % msg[1]
        sys.exit(1)

def cursor():
    return db.cursor()

def getAll(query, vals=[]):
    cursor = db.cursor()
    cursor.execute(query, vals)
    return cursor.fetchall()

def getOne(query, vals=[]):
    cursor = db.cursor()
    cursor.execute(query, vals)
    return cursor.fetchone()
