from twisted.internet import reactor
from sqlalchemy.ext.declarative import declarative_base

from aliter import log


Base = declarative_base()

class Actor(Base):
    name = Column(String)
    
    baseLevel = Column(Integer, default = 1)
    baseExp = Column(Integer)
    jobExp = Column(Integer)
    str = Column(Integer, default = 5)
    agi = Column(Integer, default = 5)
    vit = Column(Integer, default = 5)
    int = Column(Integer, default = 5)
    dex = Column(Integer, default = 5)
    luk = Column(Integer, default = 5)
    maxHP = Column(Integer, default = 40)
    hp = Column(Integer, default = 40)
    map = Column(String, default = "new_zone01")
    x = Column(Integer, default = 53)
    y = Column(Integer, default = 11)
    
    def __init__(self):
        self.direction = 0
        self.toX = self.x
        self.toY = self.y
        self.walkPath  = []
        self.walkPathOffset = 0
        self.speed = 150
        self.moveDelay = self.speed / 1000.0
    

