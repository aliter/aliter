from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from config.main import Database

engine = create_engine(Database) # TODO: Make this 4 real.
Session = sessionmaker(bind = engine)

session = Session()
