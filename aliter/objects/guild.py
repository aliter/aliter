from character import Character
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()


class Guild(Base):
    id = Column(Integer, primary_key = True)
    name = Column(String)
    masterID = Column(String, ForeignKey("users.id"))
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
    emblem = relation(GuildEmblem, backref = "guild")
    positions = relation(GuildPositions, backref = "guild")
    relations = relation(GuildRelations, backref = "guild")
    

class GuildEmblem(Base):
    """
    Guild emblems.
    """
    id = Column(Integer, primary_key = True)
    guildID = Column(Integer, ForeignKey("guilds.id"))
    data = Column(BLOB)

    guild = relation(Guild, backref="guildEmblems")


class GuildRelation(Base):
    """
    Relationships between guilds, e.g. allies/oppositions.
    """
    id = Column(Integer, primary_key = True)
    guildID = Column(Integer, ForeignKey("guilds.id"))
    relatedID = Column(Integer, ForeignKey("guilds.id"))
    type = Column(String)
    name = Column(String)

    guild = relation(Guild, backref="guildRelations")
    

class GuildPosition(Base):
    """
    Positions of a guild.
    """
    id = Column(Integer, primary_key = True)
    guildID = Column(Integer, ForeignKey("guilds.id"))
    positionID = Column(Integer)
    name = Column(String, default = "Position")
    mode = Column(Integer)
    tax = Column(Integer)

    guild = relation(Guild, backref="guildPositions")

