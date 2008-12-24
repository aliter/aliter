from sqlalchemy import *
from sqlalchemy.orm import relation, backref

from aliter.db import Base
from aliter.utils.hashcompat import sha_constructor

from character import Character


class Account(Base):
    __tablename__ = "accounts"
    
    id = Column(Integer, primary_key = True)
    username = Column(String(24))
    password = Column(String(40))
    email = Column(String(60))
    lastLogin = Column(DateTime)
    gender = Column(Boolean)
    loginCount = Column(Integer)
    gmLevel = Column(Integer)
    lastIP = Column(String(100))
    banUntil = Column(DateTime)

    characters = relation(Character, backref = "account")
    
    def __init__(self, username, password, email):
        self.username = username
        self.password = password
        self.email = email
    
    def hashPassword(self, password):
        return sha_constructor(password).hexdigest()
    
    def verifyPassword(self, password):
        if not self.password or not password:
            return False
        return self.password == self.hashPassword(password)
    

