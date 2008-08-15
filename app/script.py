import thread
    
class Script(object):
    """
    Holds a script and compiles it.
    The script will be executed in its own environment and in its own thread when the "run" method is called.
    """
    ended = False
    
    def __init__(self, code, filename = "<string>"):
        """
        Compile the script for later execution.
        """
        self.code = compile(code, filename, "exec")
    
    def run(self, actor):
        """
        Sets self.actor to "actor" (if provided) and runs "self.threadedRun" in a thread.
        If the "actor" argument is provided, the script will be executed as that player.
        """
        self.actor = actor
        thread.start_new_thread(self.threadedRun, ())
    
    def threadedRun(self):
        """
        Executes the script.
        """
        methods = dir(self)
        defaults = dir(object)
        
        for default in defaults + ["__dict__", "__module__", "__weakref__", "actor", "ended", "run", "threadedRun"]:
            methods.remove(default)
        
        environment = { "self": self.origin }
        
        for method in methods:
            environment[method] = getattr(self, method)
        
        exec self.code in environment
    
    def say(self, message):
        """
        Says something.
        """
        if self.ended:
            print "Script has already ended, so I can't say \"%s\"." % message
            return
        
        self.actor.session.sendPacket(
            0xb4,
            actorID = self.actor.gameID,
            message = str(message) + "\x00"
        )
    
    def close(self):
        """
        Shows the "close" button and ends the script's execution.
        """
        if self.ended:
            return
        
        self.actor.session.sendPacket(
            0xb6,
            actorID = self.actor.gameID
        )
        self.ended = True
    
    def menu(self, items):
        """
        Provides a menu for the player to select something from.
        The "items" argument is a dictionary containing menu item names pointing to callback functions.
        """
        if self.ended:
            return
        
        self.menuFunctions = items
        
        self.actor.session.sendPacket(
            0xb7,
            actorID = self.actor.gameID,
            items = ":".join(items.keys())
        )

    def next(self, function = None):
        """
        Shows the "Next" button and postpones the script until the player clicks it.
        """
        if self.ended:
            return
        
        self.nextFunc = function
        
        self.actor.session.sendPacket(
            0xb5,
            actorID = self.actor.gameID
        )
        
        self.actor.waitNext = True
        
        from time import sleep
        while self.actor.waitNext:
            pass # FIXME: Is there a better way to do this?
                 #        It understandably seems to slow down certain servers.
    
    def markMap(self, pointID, x, y, red = 0, green = 0, blue = 0):
        """
        Marks a coordinate on the mini-map.
        """
        self.actor.session.sendPacket(
            0x144,
            actorID = self.origin.id,
            type = 1,
            pointID = pointID,
            x = x,
            y = y,
            red = red,
            green = green,
            blue = blue
        )
    
    def removeMark(self, pointID):
        """
        Removes a marker from the mini-map.
        """
        self.actor.session.sendPacket(
            0x144,
            actorID = self.origin.id,
            type = 2,
            pointID = pointID,
            x = 0,
            y = 0,
            red = 0,
            green = 0,
            blue = 0,
        )
    
    def cutin(self, filename, position):
        """
        Displays an illustration.
        """
        self.actor.session.sendPacket(
            0x1b3,
            filename = filename,
            position = position
        )
    
    def hideCutin(self, filename):
        """
        Hides the specified cutin.
        """
        self.actor.session.sendPacket(
            0x1b3,
            filename = filename,
            position = 255
        )
    
    def hideCutins(self):
        """
        Hides all cutins.
        """
        self.actor.session.sendPacket(
            0x1b3,
            filename = "",
            position = 255
        )
