from sqlalchemy import *
from sqlalchemy.orm import relation

from aliter.db import Base


class Homunculus(Base):
    __tablename__ = "homunculi"
    
    id = Column(Integer, primary_key = True)
    characterID = Column(Integer)
    classID = Column(Integer)
    name = Column(String(24))
    level = Column(Integer)
    exp = Column(Integer)
    intimacy = Column(Integer)
    hunger = Column(Integer)
    str = Column(Integer)
    agi = Column(Integer)
    vit = Column(Integer)
    int = Column(Integer)
    dex = Column(Integer)
    luk = Column(Integer)
    hp = Column(Integer)
    maxHP = Column(Integer)
    sp = Column(Integer)
    maxSP = Column(Integer)
    skillPoint = Column(Integer)
    alive = Column(Boolean)
    vaporized = Column(Boolean)
    renameFlag = Column(Integer)
    
    

