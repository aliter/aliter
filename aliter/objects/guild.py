from sqlalchemy import *
from sqlalchemy.orm import relation, backref

from aliter.db import Base


class Guild(Base):
    __tablename__ = "guilds"
    
    id = Column(Integer, primary_key = True)
    masterID = Column(Integer)
    name = Column(String(24))
    level = Column(Integer, default = 1)
    connectedMembers = Column(Integer)
    capacity = Column(Integer)
    averageLevel = Column(Integer)
    exp = Column(Integer)
    nextExp = Column(Integer, default = 2000000)
    skillPoints = Column(Integer)
    messageTitle = Column(String(60))
    messageBody = Column(String(120))
    
    master = relation("Character", backref = "guild")
    

class GuildEmblem(Base):
    """
    Guild emblems.
    """
    __tablename__ = "guildEmblems"
    
    id = Column(Integer, primary_key = True)
    guildID = Column(Integer)
    data = Column(BLOB)

    guild = relation(Guild, backref = "emblem")
    

class GuildRelation(Base):
    """
    Relationships between guilds, e.g. allies/oppositions.
    """
    __tablename__ = "guildRelations"
    
    id = Column(Integer, primary_key = True)
    guildID = Column(Integer)
    relatedID = Column(Integer)
    name = Column(String(24))
    type = Column(Boolean)

    guild = relation(Guild, backref = backref("relations"))
    

class GuildPosition(Base):
    """
    Positions of a guild.
    """
    __tablename__ = "guildPositions"
    
    id = Column(Integer, primary_key = True)
    guildID = Column(Integer)
    positionID = Column(Integer)
    name = Column(String(24), default = "Position")
    mode = Column(Integer)
    tax = Column(Integer)

    guild = relation(Guild, backref = backref("positions"))
    

