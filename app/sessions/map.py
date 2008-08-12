from datetime import datetime

from app import log
from app.shared import config, maps
from app.objects import Session, Accounts, Characters
from app.exceptions import IllegalPacket
from app.packets import generatePacket as _, receivedPackets, sendPacket, encodePosition, encodePositionMove, decodePosition
from app.misc import getTick
from app.inter import checkLoginID, unsetLoginID, getLoginIDa, getLoginIDb
from app.event import Event

class MapSession(Session):
    def __init__(self):
        Session.__init__(self, receivedPackets['map'], log.map)
        self.lastKeepAlive = (datetime.now(), 0)
        self.npc = None
    
    def keepAlive(self, clientTick):
        if self.lastKeepAlive[1] > clientTick:
            raise IllegalPacket
        self.lastKeepAlive = (datetime.now(), clientTick)
    
    def identify(self, accountID, characterID, loginIDa):
        self.accountID   = accountID
        self.account     = Accounts.get(self.accountID)
        self.characterID = characterID
        self.character   = Characters.get(self.characterID)
        self.character.session = self
        self.id = self.character.name
        
        # Verify login
        if not checkLoginID(self.accountID, loginIDa=loginIDa):
            self.log('Invalid login ID from %s, got %s expecting %s' % (self.transport.client[0], loginIDa, getLoginIDa(self.accountID)), log.HIGH)
            unsetLoginID(self.accountID)
            self.accountID, self.characterID = None, None
            self.process = False
            return
        
        self.log('Logged in from %s, CID: %s' % (self.transport.client[0], self.character.id))
        
        # Temp: Add to actor list
        # TODO: Check to see if map is loaded
        maps[self.character.map].players[self.character.gameID] = self.character
        
        # Acknowledge login
        self.sendPacket(
            'map',
            accountID=self.accountID,
        )
        
        # Send successful packet
        self.sendPacket(
            0x73,
            tick=getTick(),
            position=encodePosition(self.character.x, self.character.y, self.character.direction),
        )
        
        # Say something
        self.sendPacket(
            0x8e,
            message = "Welcome to Aliter",
        )
        
        self.character.loadInventory()
        
        Event._registerActorView(self.character)
        Event._showActors(self.character)
    
    def move(self, position):
        if not self.character:
            raise IllegalPacket
        
        posX, posY = decodePosition(position)
        self.log('Request move to (%s, %s)' % (posX, posY), log.DEBUG)
        Event.move(self.character, posX, posY)
    
    def getActorName(self, actorID):
        if not self.character:
            raise IllegalPacket
        
        if actorID in maps[self.character.map].npcs:
            self.sendPacket(
                0x95,
                actorID = actorID,
                name = maps[self.character.map].npcs[actorID].name
            )
        
        if actorID in maps[self.character.map].players:
            self.sendPacket(
                0x95,
                actorID = actorID,
                name = maps[self.character.map].players[actorID].name
            )
    
    def npcActivate(self, npcID):
        if not self.character:
            raise IllegalPacket
        
        if npcID not in maps[self.character.map].npcs:
            return
        
        self.npc = maps[self.character.map].npcs[npcID]
        self.npc.script.ended = False
        self.npc.run(self.character)
    
    def npcNext(self, accountID):
        if not self.character:
            raise IllegalPacket
        
        self.character.waitNext = False
        
        if self.npc.script.nextFunc:
            self.npc.script.nextFunc()
    
    def npcClosed(self, accountID):
        if not self.character:
            raise IllegalPacket
        
        self.npc = None
    
    def npcMenuSelect(self, accountID, selection):
        if not self.character:
            raise IllegalPacket
        
        # They clicked "Cancel"
        if selection == 255:
            return
        
        menu = self.npc.script.menuFunctions
        selected = menu.keys()[selection - 1]
        menu[selected]()
    
    def menuButton(self, type):
        if not self.character:
            raise IllegalPacket
        
        if type:
            self.log('Returned to character select screen.', log.LOW)
            
            # Save character state
            Characters.save(self.character)
            
            # Tell others that this user has signed out, style 3 ("teleport")
            Event._sendToOtherPlayersOnMap(self.character, self.character.map, _(
                0x80,
                actorID=self.character.gameID,
                style=3
            ))
            
            # Character select screen
            self.accountID   = 0
            self.characterID = 0
            self.character   = None
            self.process     = False
            
            self.sendPacket(
                0xb3,
                type=1,
            )
        else:
            # Respawn
            
            # Set HP to 1
            self.sendPacket(
                0xb0,
                type = 5,
                value = 1 # FIXME: This should be configurable.
            )
            
            # Set SP to 1
            self.sendPacket(
                0xb0,
                type = 7,
                value = 1 # FIXME: This should be configurable.
            )
        
            self.character.load()
    
    def guildPage(self):
        if not self.character:
            raise IllegalPacket
        
        self.sendPacket(
            0x14e,
            type = 0x57 # Just say they're a member for now.
        )
    
    def guildInfo(self, page):
        if not self.character:
            raise IllegalPacket
        
        if not self.character.guildID:
            return
        
        self.sendPacket(
            0x150,
            guildID = 0,
            level = 0,
            capacity = 0,
            exp = 0,
            nextExp = 0,
            tax = 0,
            members = 0,
            name = 'Fake Guild',
            master = 'Aliter',
        )
    
    def quit(self):
        if not self.character:
            raise IllegalPacket
        
        self.log('Quitting', log.LOW)
        
        # Save character state
        Characters.save(self.character)
            
        # Tell others that this user has signed out, style 3 ("teleport")
        Event._sendToOtherPlayersOnMap(self.character, self.character.map, _(
            0x80,
            actorID=self.character.gameID,
            style=3
        ))
        
        unsetLoginID(self.accountID)
        self.accountID   = 0
        self.characterID = 0
        self.character   = None
        self.process     = False
        
        self.sendPacket(
            0x18b,
            failure=0,
        )
    
    def speech(self, message):
        if not self.character:
            raise IllegalPacket
        
        # Message format: "Character : Message\0"
        message = message.split(' : ', 1)[1][:-1]
        
        Event.sayChat(self.character, message)
    
    def emotion(self, emotion):
        if not self.character:
            raise IllegalPacket
        
        Event._sendToPlayersInSight(self.character.map, self.character.x, self.character.y, _(
            0xc0,
            actorID=self.character.gameID,
            emotion=emotion,
        ))
    
    def announce(self, message):
        Event._sendToPlayers(_(
            0x9a,
            message = message
        ))
    
    def characterName(self, characterID):
        """
        Returns the name of the character with the given ID, or "Nameless".
        """        
        player = Characters.get(characterID)
        
        if not player:
            self.log("Character %s not found." % characterID)
            self.sendPacket(
                0x194,
                actorID = 0,
                name = "Nameless"
            )
            return
        
        self.sendPacket(
            0x194,
            actorID = player.id,
            name = player.name
        )
    
    def drop(self, index, amount):
        """
        Actor has dropped an item.
        """
        if index not in self.character.inventory:
            return log.map("Character %d tried to drop an item they do not have in their inventory.", log.LOW)
        
        Event.drop(self.character, index, amount)

