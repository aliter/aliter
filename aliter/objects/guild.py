from sqlalchemy import *
from sqlalchemy.orm import relation, backref

from aliter.db import Base

from character import Character


class Guild(Base):
    __tablename__ = "guilds"
    
    id = Column(Integer, primary_key = True)
    masterID = Column(String, ForeignKey("characters.id"))
    name = Column(String)
    level = Column(Integer, default = 1)
    connectedMembers = Column(Integer)
    capacity = Column(Integer)
    averageLevel = Column(Integer)
    exp = Column(Integer)
    nextExp = Column(Integer, default = 2000000)
    skillPoints = Column(Integer)
    messageTitle = Column(String)
    messageBody = Column(String)
    
    master = relation(Character, backref = "guild")
    members = relation(Character, order_by = Character.id, backref = "guild")
    

class GuildEmblem(Base):
    """
    Guild emblems.
    """
    __tablename__ = "guildEmblems"
    
    id = Column(Integer, primary_key = True)
    guildID = Column(Integer, ForeignKey("guilds.id"))
    data = Column(BLOB)

    guild = relation(Guild, backref = "emblem")
    

class GuildRelation(Base):
    """
    Relationships between guilds, e.g. allies/oppositions.
    """
    __tablename__ = "guildRelations"
    
    id = Column(Integer, primary_key = True)
    guildID = Column(Integer, ForeignKey("guilds.id"))
    relatedID = Column(Integer, ForeignKey("guilds.id"))
    type = Column(String)
    name = Column(String)

    guild = relation(Guild, backref = backref("relations"))
    

class GuildPosition(Base):
    """
    Positions of a guild.
    """
    __tablename__ = "guildPositions"
    
    id = Column(Integer, primary_key = True)
    guildID = Column(Integer, ForeignKey("guilds.id"))
    positionID = Column(Integer)
    name = Column(String, default = "Position")
    mode = Column(Integer)
    tax = Column(Integer)

    guild = relation(Guild, backref = backref("positions"))
    

