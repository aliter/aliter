from sqlalchemy import *
from sqlalchemy.orm import relation, backref

from aliter.db import Base


class Party(Base):
    __tablename__ = "parties"
    
    id = Column(Integer, primary_key = True)
    leaderID = Column(Integer)
    name = Column(String(24))
    exp = Column(Integer)
    item = Column(Integer)
    
    leader = relation("Character", backref = "party")
    
