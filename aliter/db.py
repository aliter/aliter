from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from aliter.config import main


engine = create_engine(main.DATABASE_URI) # TODO: Make this 4 real.

Session = sessionmaker(bind = engine)
session = Session()
