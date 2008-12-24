from sqlalchemy import *
from sqlalchemy.orm import relation

from aliter.db import Base

from character import Character


class Pet(Base):
    __tablename__ = "pets"
    
    id = Column(Integer, primary_key = True)
    characterID = Column(Integer, ForeignKey("characters.id"))
    eggID = Column(Integer)
    classID = Column(Integer)
    name = Column(String)
    level = Column(Integer)
    equip = Column(Integer)
    intimacy = Column(Integer)
    hunger = Column(Integer)
    incubate = Column(Integer)
    renameFlag = Column(Boolean)
    
    character = relation(Character, backref = "pet")
    

