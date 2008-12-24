from sqlalchemy import *

from aliter.db import Base

from character import Character


class Item(Base):
    __tablename__ = "items"
    
    id = Column(Integer, primary_key = True)
    name = Column(String)
    cleanName = Column(String)
    type = Column(Integer)
    priceBuy = Column(Integer)
    priceSell = Column(Integer, default = None)
    weight = Column(Integer)
    attack = Column(Integer, default = None)
    defence = Column(Integer, default = None)
    range = Column(Integer, default = None)
    slots = Column(Integer, default = None)
    equipJobs = Column(Integer, default = 4294967295)
    equipUpper = Column(Integer, default = 7)
    equipGenders = Column(Integer, default = 2)
    equipLocations = Column(Integer, default = None)
    weaponLevel = Column(Integer, default = None)
    equipLevel = Column(Integer, default = None)
    refineable = Column(Boolean, default = None)
    view = Column(Integer, default = None)
    script = Column(String, default = None)
    equipScript = Column(String, default = None)
    unequipScript = Column(String, default = None)
    

