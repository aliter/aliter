import log

from packets import generatePacket as _
from event import EventObject, Event
from exceptions import InvalidItem, ScriptError
from shared import config, maps
from constants import *
from objects import Characters


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
        # FIXME: There's more to this.
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
        
        try:
            if select.isdigit():
                actor.give(select)
            else:
                actor.give(cleanName = select, op1 = "OR", name = select)
        except InvalidItem:
            self._gmCommandError(actor, "Item not found.")
    
    def test(self, actor):
        """docstring for test"""
        # This seems to ping the client to fill in the "Guild" setting in the status window,
        # but I can't figure out any important/unique data in this packet. Weird.
        actor.session.sendRaw("\xB0\x00\x35\x00\xE8\x02\x00\x00")
    
    def threads(self, actor):
        from time import sleep
        
        for i in range(10):
            self._gmCommandHelper(actor, "Looping %d" % i)
            sleep(1)
    
    def reloadscripts(self, actor):
        """
        Reloads every enabled script.
        """
        from aliter import Aliter
        
        aliter = Aliter()
        
        print ""
        for file in config['scripts']:
            print '\033[A\033[2KLoading scripts... %s' % file
            try:
                aliter.loadNPC(file)
            except IOError:
                print ANSI_LIGHT_RED + "\033[A\033[2KError loading script: %s [File doesn't exist]" % file + ANSI_DEFAULT
                self._gmCommandError(actor, "Error loading script: %s [File doesn't exist]" % file)
                print ''
            except ScriptError, msg:
                print ANSI_LIGHT_RED + '\nError loading script: %s [%s]' % (file, msg) + ANSI_DEFAULT
                self._gmCommandError(actor, "Error loading script: %s [%s]" % (file, msg))
                print ''
        
        self._gmCommandSuccess(actor, "Scripts reloaded.")

