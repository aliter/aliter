from datetime import datetime

from app import log
from app.shared import config, maps
from app.objects import Session, Accounts, Characters
from app.exceptions import IllegalPacket
from app.packets import receivedPackets, sendPacket, encodePosition, encodePositionMove, decodePosition
from app.misc import getTick
from app.inter import checkLoginID, unsetLoginID, getLoginIDa, getLoginIDb
from app.event import Event
from app.script import Scripts

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
            'ack',
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
            message='Welcome to Aliter',
        )
        
        # Display all actors on current map
        for npc in maps[self.character.map].npcs.values():
            self.sendPacket(
                'viewNPC',
                actorID=npc.id,
                sprite=npc.sprite,
                position=encodePosition(npc.x, npc.y, npc.dir),
            )
        for warp in maps[self.character.map].warps.values():
            self.sendPacket(
                'viewWarp',
                actorID=warp.id,
                position=encodePosition(warp.x, warp.y),
            )
    
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
                actorID=actorID,
                name=maps[self.character.map].npcs[actorID].name
            )
        if actorID in maps[self.character.map].players:
            self.sendPacket(
                0x95,
                actorID=actorID,
                name=maps[self.character.map].players[actorID].name
            )
    
    def _npcExecute(self):
        "Continue to execute an NPC script"
        while 1:
            self.npc = Scripts.execute(self.npc)
            halt     = self.npc['halt']
            if self.npc['offset'] < 0:
                self.npc = None
                halt     = True
            if halt:
                break
    def npcActivate(self, npcID):
        if not self.character:
            raise IllegalPacket
        if npcID not in maps[self.character.map].npcs:
            return
        
        npc = maps[self.character.map].npcs[npcID]
        self.log('Request to talk to "%s"' % npc.name)
        self.npc = {
            'file': npc.scriptFile,
            'offset': npc.scriptOffset,
            'register': None,
            'vars': {
                'character': self.character,
            },
            'extra': {
                'npc': npc,
                'sayName': npc.name,
            },
            'halt': False,
        }
        
        self._npcExecute()
    def npcNext(self, npcID):
        if not self.character:
            raise IllegalPacket
        
        if not self.npc \
        or self.npc['extra']['npc'].id != npcID \
        or self.npc['register'] != '_next':
            return
        
        self._npcExecute()
    def npcClosed(self, npcID):
        if not self.character:
            raise IllegalPacket
        
        if not self.npc \
        or self.npc['extra']['npc'].id != npcID \
        or self.npc['register'] not in ('_close', '_close2'):
            return
        
        if self.npc['register'] == '_close':
            self.npc = None
        else:
            self._npcExecute()
    def npcMenuSelect(self, npcID, selection):
        if not self.character:
            raise IllegalPacket
        
        if not self.npc \
        or self.npc['extra']['npc'].id != npcID \
        or self.npc['register'] not in ('_select', '_prompt'):
            return
        
        if selection == 255 and self.npc['register'] == '_select':
            # User clicked cancel
            if 'cutin' in self.npc['extra']:
                self.sendPacket(
                    0x1b3,
                    filename='',
                    position=255,
                )
            self.npc = None
        else:
            self.npc['register'] = selection
            self._npcExecute()
    
    def menuButton(self, type):
        if not self.character:
            raise IllegalPacket
        
        if type:
            self.log('Returned to character select screen.', log.LOW)
            
            # Save character state
            Characters.save(self.character)
            
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
            # Go to last save point (Revive)
            pass
    
    def guildPage(self):
        if not self.character:
            raise IllegalPacket
        
        self.sendPacket(
            0x14e,
            type=0x57,
        )
    
    def guildInfo(self, page):
        if not self.character:
            raise IllegalPacket
        
        self.sendPacket(
            0x150,
            guildID=0,
            level=0,
            capacity=0,
            exp=0,
            nextExp=0,
            tax=0,
            members=0,
            name='Fake Guild',
            master='Aliter',
        )
    
    def quit(self):
        if not self.character:
            raise IllegalPacket
        
        self.log('Quitting', log.LOW)
        
        # Save character state
        Characters.save(self.character)
        
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
        
        self.sendPacket(
            0xc0,
            actorID=self.character.gameID,
            emotion=emotion,
        )
