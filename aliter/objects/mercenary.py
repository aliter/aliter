from character import Character
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()


class Mercenary(Base):
    __tablename__ = "mercenaries"

    id = Column(Integer, primary_key = True)
    characterID = Column(Integer, ForeignKey("characters.id"))
    classID = Column(Integer)
    hp = Column(Integer, default = 1)
    sp = Column(Integer, default = 1)
    killCounter = Column(Integer)
    timeRemaining = Column(Integer)

    character = relation(Character, backref = "mercenaries")

