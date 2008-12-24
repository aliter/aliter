from sqlalchemy import create_engine, MetaData
from sqlalchemy.orm import sessionmaker
from sqlalchemy.ext.declarative import declarative_base

from aliter.config import main


engine = create_engine(main.DATABASE_URI)

meta = MetaData(engine)

Base = declarative_base(metadata = meta)

Session = sessionmaker(bind = engine)
session = Session()
