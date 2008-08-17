from datetime import datetime
from binascii import unhexlify, hexlify
from stackless import tasklet, run

from app import log
from app.shared import config, maps
from app.objects import Session, Accounts, Characters, Guilds, GuildPositions, GuildEmblems
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
            message = "Welcome to Aliter!"
        )
        
        # FIXME: Move these to the Character class so they can be used easier
        guild = self.character.guild()
        if guild:
            self.sendPacket(
                0x16c,
                guildID = guild.id,
                emblemID = guild.emblem().id,
                mode = self.character.position().mode,
                guildName = guild.name
            )
            
            self.sendPacket(
                0x16f,
                title = guild.messageTitle,
                body = guild.messageBody
            )
        
        self.character.loadInventory()
        
        self.character.online = 1
        Characters.save(self.character)
        
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
        
        if self.npc.script.nextFunc:
            self.npc.script.process = tasklet(self.npc.script.nextFunc)()
            run()
            self.npc.script.nextFunc = None
        else:
            self.npc.script.channel.send(True)
    
    def npcClosed(self, accountID):
        if not self.character:
            raise IllegalPacket
        
        if self.npc.script.closeCutinsOnPress:
            self.npc.script.closeCutins()
        
        if not self.npc.script.closeIsFake:
            self.npc = None
        else:
            self.npc.script.channel.send(True)
    
    def npcMenuSelect(self, accountID, selection):
        if not self.character:
            raise IllegalPacket
        
        # They clicked "Cancel"
        if selection == 255:
            return
        
        name, function = self.npc.script.menuFunctions[selection - 1]
        
        if function.func_code.co_argcount == 1:
            self.npc.script.process = tasklet(function)(name)
        else:
            self.npc.script.process = tasklet(function)()
        
        run()
    
    def menuButton(self, type):
        if not self.character:
            raise IllegalPacket
        
        if type:
            self.log('Returned to character select screen.', log.LOW)
            
            self.character.online = 0
            
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
    
    def guildStatus(self):
        if not self.character:
            raise IllegalPacket
        
        guild = self.character.guild()
        if not guild:
            return
        
        self.sendPacket(
            0x14e,
            type = self.character.id == guild.masterID and 0xd7 or 0x57
        )
    
    def guildInfo(self, page):
        if not self.character:
            raise IllegalPacket
        
        if not self.character.guildID:
            return
        
        guild = self.character.guild()
        
        if not guild:
            return
        
        if page == 0:
            self.sendPacket(
                0x1b6,
                guildID = guild.id,
                level = guild.level,
                online = guild.connectedMembers,
                capacity = 16 * guild.level, # FIXME: I doubt this is the formula; filler.
                averageLevel = guild.averageLevel,
                exp = guild.exp,
                nextExp = guild.nextExp,
                taxPoints = 0, # FIXME ?
                tendencyLR = 0, # FIXME eventually?
                tendencyDU = 0, # FIXME eventually?
                emblemID = guild.emblem() and guild.emblem().id or 0,
                name = guild.name,
                master = guild.master().name,
                territory = "Everywhere, bitches." # FIXME
            )
            
            relationships = []
            for relationship in guild.relations():
                relationships.append({
                    "type": relationship.type,
                    "guildID": relationship.guildID,
                    "name": relationship.name
                })
            
            self.sendPacket(
                0x14c,
                guildRelationships = relationships
            )
        elif page == 1:
            members = []
            for member in guild.members():
                members.append({
                    "accountID": member.accountID,
                    "characterID": member.id,
                    "hairStyle": member.hairStyle,
                    "hairColor": member.hairColor,
                    "gender": member.account().gender,
                    "job": member.job,
                    "baseLevel": member.baseLevel,
                    "tax": member.guildTaxed,
                    "online": member.online,
                    "positionID": member.guildPositionID - 1,
                    "name": member.name
                })
        
            self.sendPacket(
                0x154,
                guildMembers = members
            )
        elif page == 2:
            positions = []
            positionsData = []
            for position in guild.positions():
                positions.append({
                    "index": position.positionID - 1,
                    "name": position.name
                })
                
                positionsData.append({
                    "index": position.positionID - 1,
                    "mode": position.mode,
                    "tax": position.tax
                })
            
            self.sendPacket(
                0x166,
                guildPositions = positions
            )
            
            self.sendPacket(
                0x160,
                guildPositionsData = positionsData
            )
    
    def guildEmblem(self, guildID):
        """
        Sends the client the emblem for a guild.
        """
        guild = Guilds.get(guildID)
        if not guild:
            return
        
        emblem = guild.emblem()
        if not emblem:
            return
        
        self.sendPacket(
            0x152,
            guildID = guild.id,
            emblemID = emblem.id,
            emblem = unhexlify(emblem.data)
        )
    
    def guildInvite(self, accountID, inviterAccountID, characterID):
        """
        Invite a user to join a guild.
        """
        pass
    
    def setGuildEmblem(self, data):
        """
        Sets a guild's emblem.
        """
        GuildEmblems.delete(guildID = self.character.guildID)
        
        emblem = GuildEmblems.create(
            guildID = self.character.guildID,
            data = hexlify(data)
        )
        
        self.sendPacket(
            0x16c,
            guildID = self.character.guildID,
            emblemID = emblem.id,
            mode = self.character.position().mode,
            guildName = self.character.guild().name
        )
        
        self.sendPacket(
            0x152,
            guildID = self.character.guildID,
            emblemID = emblem.id,
            emblem = hexlify(data)
        )
    
    def setGuildAnnouncement(self, guildID, title, body):
        """
        Sets a guild's announcement message.
        """
        guild = Guilds.get(guildID)
        if not guild:
            return
        
        if guild.masterID != self.character.id:
            return log.map(("Character '%s' tried to change the announcement "
                            "of guild '%s' but they are not the guild master.")
                                % (self.character.name, guild.name),
                            log.HIGH)
        
        guild.messageTitle = title
        guild.messageBody = body
        Guilds.save(guild)
        
        # FIXME: This should send to all members of the guild.
        self.sendPacket(
            0x16f,
            title = title,
            body = body
        )
    
    def quit(self):
        if not self.character:
            raise IllegalPacket
        
        self.log('Quitting', log.LOW)
        
        self.character.online = 0
        
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
    
    def guildCreate(self, accountID, guildName):
        """
        Actor has attempted to create a guild.
        """
        if guildName.strip() == "":
            return
        
        # Is the player already in a guild?
        if self.character.guildID:
            return self.sendPacket(0x167, status = 1)
        
        # Does a guild already exist with this name?
        checkGuild = Guilds.get(name = guildName)
        if checkGuild:
            return self.sendPacket(0x167, status = 2)
        
        # Do they have an Emperium?
        emperium = self.character.hasItem(cleanName = "Emperium")
        if not emperium:
            return self.sendPacket(0x167, status = 3)
        
        # All is well. Create it and set their guild ID.
        
        guild = Guilds.create(
            name = guildName,
            masterID = self.character.id,
            averageLevel = self.character.baseLevel
        )
        
        # try:
        for i in xrange(1, 21):
            GuildPositions.create(
                guildID = guild.id,
                positionID = i,
                name = i == 1 and "Guild Master" or i == 20 and "Newbie" or "Position %d" % i,
                mode = i == 1 and 17 or i == 2 and 1 or 0,
                tax = 0
            )
        
        from app.objects import Inventory
        
        index = self.character.inventoryIndex(name = "Emperium")
        if emperium["stock"].amount == 1:
            Inventory.delete(emperium["stock"].id)
            del self.character.inventory[index]
        else:
            self.character.inventory[index]["stock"].amount -= 1
            emperium["stock"].amount -= 1
            Inventory.save(emperium["stock"])
        
        self.character.guildID = guild.id
        Characters.save(self.character)
        # except:
        #     Guilds.delete(guild.id)
        #     log.map("Guild creation '%s' by '%s' failed." % (guildName, self.character.name), log.HIGH)
        #     return
        
        self.sendPacket(0x167, status = 0)

