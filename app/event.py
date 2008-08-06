import random
from datetime import datetime

from twisted.internet import reactor
from twisted.internet.task import LoopingCall

import log
from shared import config, maps
from constants import *
from objects import Character
from packets import generatePacket as _, encodePositionMove
from misc import getTick, splitCommand

class EventObject(object):
    #--------------------------------------------------
    # Player finding
    #--------------------------------------------------
    
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
    
    #--------------------------------------------------
    # Chat
    #--------------------------------------------------
    
    def _gmCommandError(self, actor, message):
        actor.session.sendPacket(
            0x8e,
            message=message+'\x00',
        )
        return True
    
    def _gmRandomTile(self, map):
        # Warning: Possible lock-up
        while 1:
            x = random.randint(1, map.width-2)
            y = random.randint(1, map.height-2)
            if map.tiles[x][y] == 1:
                return x, y
    
    def _doGMCommand(self, actor, message):
        "If message is a GM command execute it"
        if message[0] == '@':
            command    = splitCommand(message)
            command[0] = command[0][1:]
            
            if command[0] == 'warp':
                if len(command) > 1:
                    if command[1] in maps:
                        if len(command) > 3:
                            x, y = int(command[2]), int(command[3])
                            if x < 0 or y < 0 \
                            or x >= maps[actor.map].width or y >= maps[actor.map].height \
                            or maps[actor.map].tiles[x][y] != 1:
                                return self._gmCommandError(actor, 'Unwalkable tile')
                        else:
                            x, y = self._gmRandomTile(maps[command[1]])
                        self.warp(actor, x, y, command[1])
                        return True
                    else:
                        return self._gmCommandError(actor, 'Invalid map')
                else:
                    return self._gmCommandError(actor, 'Format: @warp <map> [<x> <y>]')
            
            elif command[0] == 'jump':
                if len(command) > 2:
                    x, y = int(command[1]), int(command[2])
                    if x < 0 or y < 0 \
                    or x >= maps[actor.map].width or y >= maps[actor.map].height \
                    or maps[actor.map].tiles[x][y] != 1:
                        return self._gmCommandError(actor, 'Unwalkable tile')
                else:
                    x, y = self._gmRandomTile(maps[actor.map])
                self.warp(actor, x, y)
                return True
        
        return False
    
    def sayChat(self, actor, message):
        if not self._doGMCommand(actor, message):
            self._sendToOtherPlayersInSight(actor, actor.map, actor.x, actor.y, _(
                0x8d,
                actorID=actor.id,
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
                    print 'Sending movement packet to %s' % player
        
        # Is this the last one?
        if actor.walkPathOffset >= len(walkPath):
            self._movement[actor.gameID].stop()
            del self._movement[actor.gameID]
        
        if actor.x == actor.toX and actor.y == actor.toY:
            log.map('Reached (%s, %s)' % (actor.x, actor.y), log.DEBUG, id=actor.name)
    
    def move(self, actor, x, y):
        walkPath = maps[actor.map].pathfind(actor.x, actor.y, x, y)
        actor.walkPath = walkPath
        actor.walkPathOffset = 0
        
        if actor.gameID in self._movement:
            # Stop actor if already moving
            self._movement[actor.gameID].stop()
        self._movement[actor.gameID] = LoopingCall(self._moveLoop, actor, walkPath)
        self._movement[actor.gameID].start(actor.moveDelay, now=True)
    
    def warp(self, actor, x, y, map=None):
        if map:
            if map not in maps:
                return False # TODO: Handle this properly
            del maps[actor.map].players[actor.gameID]
            actor.map = map
            maps[actor.map].players[actor.gameID] = actor
        actor.x, actor.y = x, y
        actor.session.sendPacket(
            0x91,
            map=actor.map+'.gat',
            x=actor.x,
            y=actor.y,
        )
        # TODO: Send warp effect to other players in sight
    
    #--------------------------------------------------
    # Battle
    #--------------------------------------------------
    
    def damage(self, target):
        pass

Event = EventObject()
