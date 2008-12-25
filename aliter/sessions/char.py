from socket import inet_aton
from sqlalchemy import and_

from aliter import log
from aliter.config.main import MAP_SERVER
from aliter.objects.servers import Session
from aliter.objects.account import Account
from aliter.objects.character import Character
from aliter.exceptions import IllegalPacket
from aliter.packets import receivedPackets, sendPacket
from aliter.inter import checkLoginID, unsetLoginID, getLoginIDa, getLoginIDb


class CharSession(Session):
    def __init__(self):
        Session.__init__(self, receivedPackets['char'], log.char)
    
    def identify(self, accountID, loginIDa, loginIDb, gender):
        self.accountID = accountID
        
        # Verify login
        if not checkLoginID(self.accountID, loginIDa, loginIDb):
            print loginIDa, loginIDb, getLoginIDa(self.accountID), getLoginIDb(self.accountID)
            self.log('Invalid login ID from %s, got %s/%s expecting %s/%s' % (self.transport.client[0], loginIDa, loginIDb, getLoginIDa(self.accountID), getLoginIDb(self.accountID)), log.HIGH)
            unsetLoginID(self.accountID)
            self.failed(0x6c) # Error getting characters
            self.accountID, self.characterID = None, None
            self.process = False
            return
        
        self.account = session.query(Account).filter(Account.id == accountID).one()
        self.id = self.account.username
        self.log('Logged in from %s, AID: %s' % (self.transport.client[0], self.account.id), log.LOW)
        
        # Acknowledge login
        self.sendPacket(
            'ack',
            accountID = self.accountID,
        )
        
        # Build character packet stuff
        chars = self.account.characters
        charPack = []
        for char in chars:
            packetChar = char.__dict__
            packetChar['speed'] = int(char.speed)
            charPack.append(packetChar)
        
        self.sendPacket(
            0x6b,
            character = charPack
        )
    
    def selectChar(self, charNum):
        if not self.accountID:
            raise IllegalPacket
        
        # Load character and keep in memory
        char = session.query(Character).filter(and_(Character.accountID == self.accountID, Character.charNum == charNum))
        
        if not char: # Something went wrong
            self.sendPacket(
                0x6c,
                type = 0x00,
            )
            self.log('Character selection failed.', log.LOW)
            return
        
        self.log('Selected %s, sending map server details.' % char.name, log.LOW)
        self.sendPacket(
            0x71,
            characterID = char.id,
            map = char.map+'.gat',
            ip = inet_aton(MAP_SERVER[0]['address']['host']),
            port = MAP_SERVER[0]['address']['port'],
        )
    
    def createChar(self, name, str, agi, vit, int, dex, luk, charNum, hairStyle, hairColor):
        if not self.accountID:
            raise IllegalPacket
        
        hairColor = hairColor - 1
        
        self.log('Request character creation - Name: %s, Position: %s, Hair: %s/%s, Stats: %s/%s/%s/%s/%s/%s' % (name, charNum, hairStyle, hairColor, str, agi, vit, int, dex, luk,), log.LOW)
        
        char = Characters(
            accountID = self.accountID,
            charNum = charNum,
            name = name,
            str = str,
            agi = agi,
            vit = vit,
            int = int,
            dex = dex,
            luk = luk,
            hairStyle = hairStyle, 
            hairColor = hairColor,
        )
        session.add(char)
        session.commit()
        
        if char:
            # Character created
            packetChar = char.__dict__
            packetChar['speed'] = packetChar['speed'] + 10
            self.sendPacket(
                0x6d,
                character = [packetChar],
            )
            self.log('Character creation successful.', log.LOW)
        else:
            # Character creation failed
            self.sendPacket(
                0x6e,
                type = 0x02,
            )
            self.log('Character creation denied.', log.LOW)
    
    def deleteChar(self, characterID, Email):
        if not self.accountID:
            raise IllegalPacket
        
        char = session.query(Character).filter(Character.id == characterID)
        session.delete(char)
        self.log('Request deletion of %s' % char.name, log.LOW)
        
        if char.accountID != self.accountID:
            self.sendPacket(
                0x70,
                type = 0x01,
            )
            self.log('Character deletion denied - Incorrect account.', log.LOW)
            return
        if Email != self.account.Email:
            self.sendPacket(
                0x70,
                type = 0x00,
            )
            self.log('Character deletion denied - Incorrect E-mail (Got "%s", expecting "%s").' % (Email, self.account.Email), log.LOW)
            return
        if not Characters.delete(char.id):
            self.sendPacket(
                0x70,
                type = 0x01,
            )
            self.log('Character deletion denied - Error deleting.', log.LOW)
            return
        self.sendPacket(
            0x6f,
        )
        self.log('Character deletion successful.', log.LOW)
        return
    
    def keepAlive(self, kaAccountID):
        if not self.accountID:
            raise IllegalPacket
        
        if kaAccountID != self.accountID:
            self.log('Keep-alive returned account %s, killing.' % (self.accountID, kaAccountID), log.LOW)
            self.process = False
    
    def failed(self, type):
        self.sendPacket(
            0x6c,
            type = type
        )
    
