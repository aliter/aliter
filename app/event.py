import sys
import random
import thread
from datetime import datetime

from twisted.internet import reactor
from twisted.internet.task import LoopingCall

import log
from shared import config, maps
from constants import *
from objects import Characters, Accounts
from packets import generatePacket as _, encodePositionMove, encodePosition, decodePosition
from misc import getTick, splitCommand


class EventObject(object):
    #--------------------------------------------------
    # Player finding
    #--------------------------------------------------
    
    def _getPlayer(self, match, value):
        for map in maps:
            for player in maps[map].players.itervalues():
                if getattr(player, match) == value:
                    return player
    
    def _playersInSight(self, map, x, y):
        players = []
        for player in maps[map].players.itervalues():
            if  player.x - x < config['MapServer'][0]['visible'] \
            and player.x - x > -config['MapServer'][0]['visible'] \
            and player.y - y < config['MapServer'][0]['visible'] \
            and player.y - y > -config['MapServer'][0]['visible']:
                players.append(player)
        
        return players
    
    def _playersOnMap(self, map):
        players = []
        for player in maps[map].players.itervalues():
            players.append(player)
        return players
    
    #--------------------------------------------------
    # Packet sending
    #--------------------------------------------------
    
    def _sendToPlayers(self, packet):
        for map in maps:
            for player in maps[map].players.itervalues():
                player.session.sendRaw(packet)
    
    def _sendToPlayersInSight(self, map, x, y, packet):
        for player in self._playersInSight(map, x, y):
            player.session.sendRaw(packet)
    
    def _sendToOtherPlayersInSight(self, actor, map, x, y, packet):
        for player in self._playersInSight(map, x, y):
            if player.id != actor.id:
                player.session.sendRaw(packet)
    
    def _sendToPlayersOnMap(self, map, packet):
        for player in self._playersOnMap(map):
            player.session.sendRaw(packet)
    
    def _sendToOtherPlayersOnMap(self, actor, map, packet):
        for player in self._playersOnMap(map):
            if player.id != actor.id:
                player.session.sendRaw(packet)
    
    def _showActors(self, actor):
        for player in self._playersInSight(actor.map, actor.x, actor.y):
            if player.id != actor.id:
                actor.session.sendRaw(_(
                    0x1d7,
                    accountID=player.accountID,
                    equip=2,
                    w1=player.viewWeapon,
                    w2=player.viewShield
                ))
        
                # FIXME: Should this use _sendToOtherPlayersOnMap?
                actor.session.sendRaw(_(
                    0x22c,
                    accountID=player.accountID,
                    speed=150, # TODO: Make this real.
                    opt1=0, # TODO: Make this real.
                    opt2=0, # TODO: Make this real.
                    opt3=0, # TODO: Make this real.
                    job=player.job,
                    hstyle=player.hairStyle,
                    weapon=player.viewWeapon,
                    shield=player.viewShield,
                    lowhead=player.viewHeadBottom,
                    tick=getTick(),
                    tophead=player.viewHeadTop,
                    midhead=player.viewHeadMiddle,
                    hcolor=player.hairColor,
                    ccolor=player.clothesColor,
                    headdir=0, # FIXME: How can this be tested?
                    guildID=player.guildID,
                    guildEmblem=0, # TODO: Make this real.
                    manner=0, # TODO: Make this real.
                    effect=0, # TODO: Make this real.
                    karma=0, # TODO: Make this real.
                    gender=player.account.gender,
                    position=encodePosition(player.x, player.y) + "\x88\x05\x05",
                    blevel=player.baseLevel,
                ))
        
        # Display all NPCs on current map
        for npc in maps[actor.map].npcs.values():
            actor.session.sendPacket(
                'viewNPC',
                actorID = npc.id,
                sprite = npc.sprite,
                position = encodePosition(npc.x, npc.y, npc.dir),
            )
        
        # Display warps
        for warp in maps[actor.map].warps.values():
            actor.session.sendPacket(
                'viewWarp',
                actorID = warp.id,
                position = encodePosition(warp.x, warp.y),
            )
    
    def _registerActorView(self, actor):
        self._sendToOtherPlayersInSight(actor, actor.map, actor.x, actor.y, _(
            0x1d7,
            accountID=actor.accountID,
            equip=2,
            w1=actor.viewWeapon,
            w2=actor.viewShield
        ))
        
        account = Accounts.get(actor.accountID)
        
        if account.gender == 1:
            gender = 1
        else:
            gender = 0
        
        # FIXME: Should this use _sendToOtherPlayersOnMap?
        self._sendToOtherPlayersInSight(actor, actor.map, actor.x, actor.y, _(
            0x22b,
            accountID=actor.accountID,
            speed=150, # TODO: Make this real.
            opt1=0, # TODO: Make this real.
            opt2=0, # TODO: Make this real.
            opt3=0, # TODO: Make this real.
            job=actor.job,
            hstyle=actor.hairStyle,
            weapon=actor.viewWeapon,
            shield=actor.viewShield,
            lowhead=actor.viewHeadBottom,
            tophead=actor.viewHeadTop,
            midhead=actor.viewHeadMiddle,
            hcolor=actor.hairColor,
            ccolor=actor.clothesColor,
            headdir=0, # FIXME: How can this be tested?
            guildID=actor.guildID,
            guildEmblem=0, # TODO: Make this real.
            manner=0, # TODO: Make this real.
            effect=0, # TODO: Make this real.
            karma=0, # TODO: Make this real.
            gender=gender,
            position=encodePosition(actor.x, actor.y) + "\x05\x05",
            blevel=actor.baseLevel,
        ))
    
    #--------------------------------------------------
    # Chat
    #--------------------------------------------------
    
    def _gmCommandHelper(self, actor, message):
        """
        Display a help/usage message to the player, in light green.
        """
        actor.session.sendPacket(
            0x17f,
            message = message + "\x00"
        )
        return True
    
    def _gmCommandError(self, actor, message):
        """
        Display an error message to the player, in red. (FIXME: Is it possible to put it in red? If not, party chat colour?)
        """
        actor.session.sendPacket(
            0x8e,
            message = message + "\x00"
        )
        return False
    
    def _gmCommandSuccess(self, actor, message):
        """
        Display a success message to the player, in green.
        """
        actor.session.sendPacket(
            0x8e,
            message = message + "\x00"
        )
        return False
    
    def _gmRandomTile(self, map):
        # Warning: Possible lock-up
        while 1:
            x = random.randint(1, map.width-2)
            y = random.randint(1, map.height-2)
            if map.tiles[x][y] == 1:
                return x, y
    
    def _doGMCommand(self, actor, message):
        """
        If message is a GM command (starts with @), try to execute it.
        """
        if message[0] != '@':
            return False
        
        commands   = splitCommand(message)
        arguments  = commands[1:]
        command    = commands[0][1:]
        
        gm = GMCommand()
        
        if (hasattr(gm, command) == False):
            return False
        
        attr = getattr(gm, command)
        
        if not callable(attr):
            return False
        
        arguments.insert(0, actor)
        
        # Don't pass too many arguments (-1 is for "self")
        arguments = arguments[:(attr.func_code.co_argcount - 1)]
        
        thread.start_new_thread(attr, tuple(arguments))
        
        return True
    
    def sayChat(self, actor, message):
        if not self._doGMCommand(actor, message):
            self._sendToOtherPlayersInSight(actor, actor.map, actor.x, actor.y, _(
                0x8d,
                actorID=actor.gameID,
                message='%s : %s' % (actor.name, message+'\x00'),
            ))
            
            actor.session.sendPacket(
                0x8e,
                message='%s : %s' % (actor.name, message+'\x00'),
            )
    
    def sayParty(self, actor, message):
        pass
    
    def sayGuild(self, actor, message):
        pass
    
    def sayGM(self, actor, message, type):
        pass
    
    #--------------------------------------------------
    # Movement
    #--------------------------------------------------
    
    _movement = {}
    def _moveLoop(self, actor, walkPath):
        actor.x, actor.y, nextKey = walkPath[actor.walkPathOffset]
        actor.walkPathOffset += 1
        
        if nextKey:
            actor.toX, actor.toY = nextKey[0], nextKey[1]
            for player in self._playersInSight(actor.map, actor.x, actor.y):
                if player == actor:
                    player.session.sendPacket(
                        0x87,
                        actorID=actor.gameID,
                        position=encodePositionMove(actor.x, actor.y, actor.toX, actor.toY),
                        tick=getTick(),
                    )
                else:
                    player.session.sendPacket(
                        0x86,
                        actorID=actor.gameID,
                        position=encodePositionMove(actor.x, actor.y, actor.toX, actor.toY) + "\x88",
                        tick=getTick(),
                    )
                    player.session.sendPacket(
                        0x7f,
                        tick=getTick(),
                    )
                    
        
        # Is this the last one?
        if actor.walkPathOffset >= len(walkPath):
            self._movement[actor.gameID].stop()
            del self._movement[actor.gameID]
        
        if actor.x == actor.toX and actor.y == actor.toY:
            log.map('Reached (%s, %s)' % (actor.x, actor.y), log.DEBUG, id=actor.name)
    
    def move(self, actor, x, y):
        """
        Actor has moved.
        """
        walkPath = maps[actor.map].pathfind(actor.x, actor.y, x, y)
        actor.walkPath = walkPath
        actor.walkPathOffset = 0
        
        if actor.gameID in self._movement and self._movement[actor.gameID].running:
            # Stop actor if already moving
            self._movement[actor.gameID].stop()
        
        self._movement[actor.gameID] = LoopingCall(self._moveLoop, actor, walkPath)
        self._movement[actor.gameID].start(actor.moveDelay, now=True)
    
    def warp(self, actor, x, y, map=None):
        """
        Actor has warped.
        """
        if map:
            if map not in maps:
                log.map("%s tried to warp to invalid map: %s" % (actor.name, map), log.HIGH)
                return False # TODO: Handle this properly
            
            del maps[actor.map].players[actor.gameID]
            actor.map = map
            maps[actor.map].players[actor.gameID] = actor
        
        actor.x, actor.y = x, y
        
        actor.session.sendPacket(
            0x91,
            map = actor.map+'.gat',
            x = actor.x,
            y = actor.y,
        )
        
        self._registerActorView(actor)
        self._showActors(actor)
        # TODO: Send warp effect to other players in sight
    
    def drop(self, actor, index, amount):
        """
        Actor has dropped an item.
        """
        inventory = actor.inventory[index]
        item, stock = inventory["item"], inventory["stock"]
        
        subX = (random.randint(0, sys.maxint) & 3) * 3 + 3
        subY = ((random.randint(0, sys.maxint) >> 2) & 3) * 3 + 3
        
        maps[actor.map].objects += 1
        
        self._sendToPlayersOnMap(actor.map, _(
            0x9e,
            objectID = maps[actor.map].objects,
            itemID = item.id,
            identified = stock.identified,
            x = actor.x,
            y = actor.y - 1, # FIXME: Replace this with official behaviour.
            subX = subX,
            subY = subY,
            amount = amount
        ))
        
        actor.session.sendPacket(
            0xaf,
            index = index,
            amount = amount
        )
        
        from objects import Inventory
        
        if amount == stock.amount:
            Inventory.delete(stock.id)
            del actor.inventory[index]
        else:
            actor.inventory[index]["stock"].amount -= amount
            stock.amount -= amount
            Inventory.save(stock)
        
        reactor.callLater(60, self.removeDrop, actor.map, maps[actor.map].objects)
    
    def removeDrop(self, map, objectID):
        """
        Removes a dropped item from the map.
        """
        log.map("Removing unpicked item %d from map %s." % (objectID, map), log.DEBUG)
        self._sendToPlayersOnMap(map, _(
            0xa1,
            objectID = objectID
        ))
        
        maps[map].objects -= 1
    
    #--------------------------------------------------
    # Battle
    #--------------------------------------------------
    
    def damage(self, target):
        pass

Event = EventObject()

class GMCommand(EventObject):
    loops = []
    
    def warp(self, actor, map = None, x = 0, y = 0):
        """
        Warps the player to a specified map, x, and y corrdinate.
        """
        if map == None:
            return self._gmCommandHelper(actor, "Usage: @warp <map> [<x> <y>]")
        
        if map not in maps:
            return self._gmCommandError(actor, "Invalid map.")
        
        if x != 0 or y != 0:
            x, y = int(x), int(y)
            if x < 0 or y < 0 \
            or x >= maps[map].width or y >= maps[map].height \
            or maps[map].tiles[x][y] != 1:
                return self._gmCommandError(actor, "Unwalkable tile.")
        else:
            x, y = self._gmRandomTile(maps[map])
        
        Event.warp(actor, x, y, map)
    
    def jump(self, actor, x = 0, y = 0):
        """
        Jumps the player to either a specified coordinate or a random position.
        """
        if x != 0 or y != 0:
            x, y = int(x), int(y)
            if x < 0 or y < 0 \
            or x >= maps[actor.map].width or y >= maps[actor.map].height \
            or maps[actor.map].tiles[x][y] != 1:
                return self._gmCommandError(actor, "Unwalkable tile.")
        else:
            x, y = self._gmRandomTile(maps[actor.map])
        
        Event.warp(actor, x, y)
        self._gmCommandSuccess(actor, "Jumped to %d %d." % (x, y))
    
    def refresh(self, actor):
        """
        Refreshes the player's screen.
        """
        Event.warp(actor, actor.x, actor.y)
    
    def me(self, actor, *words):
        """
        Outputs what they say after @me as a pseudo-event message, a-la IRC.
        """
        message = " ".join(words)
        
        if message.strip() == "":
            return self._gmCommandHelper(actor, "Usage: @me <action>")
        
        self._sendToOtherPlayersInSight(actor, actor.map, actor.x, actor.y, _(
            0x8d,
            actorID=actor.gameID,
            message='* %s %s' % (actor.name, message+'\x00'),
        ))
        
        actor.session.sendPacket(
            0x8e,
            message='* %s %s' % (actor.name, message+'\x00'),
        )

    def kick(self, actor, name = None, *reason):
        """
        Kicks a user off, searches by their name.
        """
        if name == None:
            return self._gmCommandHelper(actor, "Usage: @kick <name> [<reason>]")
        
        from app.inter import unsetLoginID
        
        reason = " ".join(reason)
        
        if reason.strip() == "":
            log.map("Kicking user %s (%s)" % (name, actor.name), log.LOW)
        else:
            log.map("Kicking user %s (%s - %s)" % (name, reason, actor.name), log.LOW)
        
        player = self._getPlayer("name", name)
        
        if not player:
            return self._gmCommandError(actor, "User '%s' not found." % name)
        
        # Save character state
        Characters.save(player)
            
        # Tell others that this user has signed out, style 3 ("teleport")
        self._sendToOtherPlayersOnMap(player, player.map, _(
            0x80,
            actorID=player.gameID,
            style=3
        ))
        
        unsetLoginID(player.accountID)
        
        player.session.sendPacket(
            0x81,
            type=15,
        )
        
        actor.session.sendPacket(0xcd) # "User Killed/Disconnected"
    
    def effect(self, actor, id = None):
        """
        Shows a status effect to the client and characters in sight.
        """
        if id == None:
            return self._gmCommandHelper(actor, "Usage: @effect <id>")
        
        # FIXME: Should this use _sendToPlayersOnMap?
        self._sendToPlayersInSight(actor.map, actor.x, actor.y, _(
            0x1f3,
            accountID = actor.accountID,
            effect = int(id)
        ))
    
    def die(self, actor):
        """
        Kills yourself.
        """
        self.effect(actor, 450)
        
        self._sendToPlayersOnMap(actor.map, _(
            0x80,
            actorID = actor.gameID,
            style = 1
        ))
        
        actor.session.sendPacket(
            0xb0,
            type = 5,
            value = 0
        )
        
        actor.session.sendPacket(
            0xb0,
            type = 7,
            value = 0
        )
        
        self._gmCommandSuccess(actor, "You have died.")
    
    def kill(self, actor, name = None):
        """
        Kills a user.
        """
        if name == None:
            return self._gmCommandHelper(actor, "Usage: @kill <name>")
        
        player = self._getPlayer("name", name)
        
        if not player:
            return self._gmCommandError(actor, "User '%s' not found." % name)
        
        self.effect(player, 450)
        
        self._sendToPlayersOnMap(player.map, _(
            0x80,
            actorID = player.gameID,
            style = 1
        ))
        
        player.session.sendPacket(
            0xb0,
            type = 5,
            value = 0
        )
        
        player.session.sendPacket(
            0xb0,
            type = 7,
            value = 0
        )
        
        self._gmCommandSuccess(actor, "Player '%s' killed." % player.name)
    
    def load(self, actor):
        """
        Loads the player to their save point.
        """
        actor.load()
    
    def save(self, actor):
        """
        Saves the player's current position.
        """
        actor.save(actor.map, actor.x, actor.y)
        Characters.save(actor)
        self._gmCommandSuccess(actor, "Current location set as save point.")
    
    def item(self, actor, select = None, amount = 1):
        """
        Gives the player the specified item.
        """
        if select == None:
            return self._gmCommandHelper(actor, "Usage: @item <id/name> [<amount>]")
        
        from objects import Items, Inventory
        
        if select.isdigit():
            item = Items.get(select)
        else:
            item = Items.get(cleanName = select, op1 = "OR", name = select)
        
        if not item:
            return self._gmCommandError(actor, "Item not found.")
        
        inventory = Inventory.getAll(
            characterID = actor.id
        )
        
        if item.equipLocations == None:
            # Do they already have this item?
            stock = Inventory.get(itemID = item.id)
            
            if stock and stock:
                stock.amount += int(amount)
                Inventory.save(stock)
            else:
                stock = Inventory.create(
                    characterID = actor.id,
                    itemID = item.id,
                    amount = int(amount)
                )
            
            search = [k for k, v in actor.inventory.iteritems() if v["item"].id == item.id]
            index = search and search[0] or len(inventory) + 2
            
            actor.session.sendPacket(
                0xa0,
                index = index,
                amount = int(amount),
                itemID = item.id,
                identified = 1,
                broken = 0,
                refine = 0,
                card1 = 0,
                card2 = 0,
                card3 = 0,
                card4 = 0,
                equipLocations = item.equipLocations or 0,
                type = item.type,
                fail = 0
            )
            
            actor.inventory[index] = { "item": item, "stock": stock }
        else:
            for x in xrange(int(amount)):
                stock = Inventory.create(
                    characterID = actor.id,
                    itemID = item.id,
                    amount = 1
                )
                
                index = len(inventory) + 2 + x
                actor.session.sendPacket(
                    0xa0,
                    index = index,
                    amount = 1,
                    itemID = item.id,
                    identified = 1,
                    broken = 0,
                    refine = 0,
                    card1 = 0,
                    card2 = 0,
                    card3 = 0,
                    card4 = 0,
                    equipLocations = item.equipLocations or 0,
                    type = item.type,
                    fail = 0
                )
                
                actor.inventory[index] = { "item": item, "stock": stock }
    
    def test(self, actor):
        """docstring for test"""
        # This seems to ping the client to fill in the "Guild" setting in the status window,
        # but I can't figure out any important/unique data in this packet. Weird.
        actor.session.sendRaw("\xB0\x00\x35\x00\xE8\x02\x00\x00")
    
    # def threads(self, actor, start = 0):
    #     from time import sleep
    #     
    #     if start == 0:
    #         log.map("Running @test...")
    #         if "test" not in self.loops:
    #             self.loops.append("test")
    #             log.map("Adding 'test' to the loops list.")
    #         else:
    #             log.map("Removing 'test' from the loops list.")
    #             return self.loops.remove("test")
    #     
    #     if "test" not in self.loops or start >= 10:
    #         return
    #     
    #     if start != 0:
    #         sleep(1)
    #     
    #     log.map("Looping: %s" % start, log.HIGH)
    #     self._gmCommandError(actor, "Looping: "+ str(start))
    #     self.threads(actor, start + 1)

