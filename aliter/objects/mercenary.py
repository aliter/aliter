from sqlalchemy import *
from sqlalchemy.orm import relation

from aliter.db import Base


class Mercenary(Base):
    __tablename__ = "mercenaries"
    
    id = Column(Integer, primary_key = True)
    characterID = Column(Integer)
    classID = Column(Integer)
    hp = Column(Integer, default = 1)
    sp = Column(Integer, default = 1)
    killCounter = Column(Integer)
    timeRemaining = Column(Integer)
    

