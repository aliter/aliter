import sys
import random
import thread
from datetime import datetime

from twisted.internet import reactor
from twisted.internet.task import LoopingCall
from stackless import channel, tasklet, run, schedule

import log
from shared import config, maps
from constants import *
from aliter.exceptions import ScriptError
from packets import generatePacket as _, encodePositionMove, encodePosition, decodePosition
from misc import getTick, splitCommand


class EventObject(object):
    #--------------------------------------------------
    # Player finding
    #--------------------------------------------------
    
    def _getPlayer(self, match, value):
        """
        Returns a player map instance, searching by `match` Player attribute == `value`.
        """
        for map in maps:
            for player in maps[map].players.itervalues():
                if getattr(player, match) == value:
                    return player
    
    def _playersInSight(self, map, x, y):
        """
        Returns a list of all players in sight of a given map and coordinates.
        """
        players = []
        for player in maps[map].players.itervalues():
            if  player.x - x < config['MapServer'][0]['visible'] \
            and player.x - x > -config['MapServer'][0]['visible'] \
            and player.y - y < config['MapServer'][0]['visible'] \
            and player.y - y > -config['MapServer'][0]['visible']:
                players.append(player)
        
        return players
    
    def _playersOnMap(self, map):
        """
        Returns a list of all players on a given map.
        """
        players = []
        for player in maps[map].players.itervalues():
            players.append(player)

        return players
    
    #--------------------------------------------------
    # Packet sending
    #--------------------------------------------------
    
    def _sendToPlayers(self, packet):
        """
        Sends a packet to every player on the server.
        """
        for map in maps:
            for player in maps[map].players.itervalues():
                player.session.sendRaw(packet)
    
    def _sendToPlayersInSight(self, map, x, y, packet):
        """
        Sends a packet to every player visible from the given map and coordinates.
        """
        for player in self._playersInSight(map, x, y):
            player.session.sendRaw(packet)
    
    def _sendToOtherPlayersInSight(self, actor, map, x, y, packet):
        """
        Same as _sendToPlayersInSight, but does not send to the given actor.
        """
        for player in self._playersInSight(map, x, y):
            if player.id != actor.id:
                player.session.sendRaw(packet)
    
    def _sendToPlayersOnMap(self, map, packet):
        """
        Sends a packet to every player on a map.
        """
        for player in self._playersOnMap(map):
            player.session.sendRaw(packet)
    
    def _sendToOtherPlayersOnMap(self, actor, map, packet):
        """
        Same as _sendToPlayersOnMap, but does not send to the given actor.
        """
        for player in self._playersOnMap(map):
            if player.id != actor.id:
                player.session.sendRaw(packet)
    
    def _showActors(self, actor):
        """
        Sends information of every player and NPC within the given actor's visible range.
        """
        for player in self._playersInSight(actor.map, actor.x, actor.y):
            if player.id != actor.id:
                actor.session.sendPacket(
                    0x1d7,
                    accountID=player.accountID,
                    equip=2,
                    w1=player.viewWeapon,
                    w2=player.viewShield
                )
                
                actor.session.sendPacket(
                    0x195,
                    accountID = player.accountID,
                    name = player.name,
                    partyName = "", # TODO: Implement parties. :P
                    guildName = player.guild() and player.guild().name or "",
                    guildPosition = player.position() and player.position().name or ""
                )
                
                actor.session.sendPacket(
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
                    guildEmblem=player.guild() and player.guild().emblem().id or 0,
                    manner=0, # TODO: Make this real.
                    effect=0, # TODO: Make this real.
                    karma=0, # TODO: Make this real.
                    gender=player.account().gender,
                    position=encodePosition(player.x, player.y) + "\x88\x05\x05",
                    blevel=player.baseLevel,
                )
        
        # Display all NPCs on current map
        for npc in maps[actor.map].npcs.values():
            actor.session.sendPacket(
                'viewNPC',
                actorID = npc.num,
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
        """
        Sends the given actor's view to every player within sight.
        """
        self._sendToOtherPlayersInSight(actor, actor.map, actor.x, actor.y, _(
            0x1d7,
            accountID=actor.accountID,
            equip=2,
            w1=actor.viewWeapon,
            w2=actor.viewShield
        ))
        
        # Update their name
        # FIXME: Should this use _sendToOtherPlayersOnMap?
        self._sendToPlayersInSight(actor.map, actor.x, actor.y, _(
            0x195,
            accountID = actor.accountID,
            name = actor.name,
            partyName = "", # TODO: Implement parties. :P
            guildName = actor.guild() and actor.guild().name or "",
            guildPosition = actor.position() and actor.position().name or ""
        ))
        
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
            guildEmblem=actor.guild() and actor.guild().emblem().id or 0,
            manner=0, # TODO: Make this real.
            effect=0, # TODO: Make this real.
            karma=0, # TODO: Make this real.
            gender=actor.account().gender,
            position=encodePosition(actor.x, actor.y) + "\x05\x05",
            blevel=actor.baseLevel
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
        """
        Returns a random pair of walkable corrdinates on a given map.
        """
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
        
        from gm import GMCommand
        
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
        # tasklet(attr)(*arguments)
        # run()
        
        return True
    
    def sayChat(self, actor, message):
        """
        Send appropriate chat packets if the message does not appear to be a GM command.
        """
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
