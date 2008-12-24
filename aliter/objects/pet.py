from sqlalchemy import *
from sqlalchemy.orm import relation

from aliter.db import Base


class Pet(Base):
    __tablename__ = "pets"
    
    id = Column(Integer, primary_key = True)
    characterID = Column(Integer)
    eggID = Column(Integer)
    classID = Column(Integer)
    name = Column(String(24))
    level = Column(Integer)
    equip = Column(Integer)
    intimacy = Column(Integer)
    hunger = Column(Integer)
    incubate = Column(Integer)
    renameFlag = Column(Boolean)
    

