from aliter.utils.hashcompat import sha_constructor
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()

class Account(Base):
    __tablename__ = "accounts"

    id = Column(Integer, primary_key = True)
    username = Column(String)
    password = Column(String)
    email = Column(String)
    lastLogin = Column(DateTime)
    gender = Column(Binary)
    loginCount = Column(Integer)
    gmLevel = Column(Integer)
    lastIP = Column(String)
    banUntil = Column(DateTime)

    def __init__(self, username, password, email)
        self.username = username
        self.password = password
        self.email = email
    
    def hashPassword(self, password):
        return sha_constructor(password).hexdigest()
    
    def verifyPassword(self, password):
        if not self.password or not password:
            return False
        
        return self.password == self.hashPassword(password)

