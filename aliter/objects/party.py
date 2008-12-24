from sqlalchemy import *
from sqlalchemy.orm import relation, backref

from aliter.db import Base

from character import Character


class Party(Base):
    __tablename__ = "parties"
    
    id = Column(Integer, primary_key = True)
    leaderID = Column(String, ForeignKey("characters.id"))
    name = Column(String)
    exp = Column(Boolean)
    item = Column(Boolean)
    
    leader = relation(Character, backref = "party")
    members = relation(Character, order_by = Character.id, backref = "party")
    
