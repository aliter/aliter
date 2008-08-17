import thread
from stackless import channel, tasklet, run
    
class Script(object):
    """
    Holds a script and compiles it.
    The script will be executed in its own environment and in its own thread when the "run" method is called.
    """
    process = False
    channel = channel()
    
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
        Runs the script in a tasklet and sets the process.
        """
        self.process = tasklet(self.tasklet)()
        run()
    
    def tasklet(self):
        """
        Executes the script.
        """
        methods = dir(self)
        defaults = dir(object)
        
        for default in defaults + ["__dict__", "__module__", "__weakref__", "process", "run", "threadedRun", "tasklet"]:
            methods.remove(default)
        
        environment = { "self": self.origin }
        
        for method in methods:
            environment[method] = getattr(self, method)
        
        exec self.code in environment
    
    def say(self, *messages):
        """
        Says something.
        """
        for message in messages:
            if message != None and message != False:
                self.actor.session.sendPacket(
                    0xb4,
                    actorID = self.actor.gameID,
                    message = str(message) + "\x00"
                )
    
    def close(self, closeCutins = True):
        """
        Shows the "close" button and ends the script's execution.
        """
        self.closeIsFake = False
        
        self.actor.session.sendPacket(
            0xb6,
            actorID = self.actor.gameID
        )
        
        if closeCutins:
            self.closeCutins()
        
        self.process.kill()
    
    def fakeClose(self):
        """
        Shows the "close" button and ends the script's execution.
        """
        self.closeIsFake = True
        
        self.actor.session.sendPacket(
            0xb6,
            actorID = self.actor.gameID
        )
        
        self.channel.receive()
    
    def menu(self, *items):
        """
        Provides a menu for the player to select something from.
        The "items" argument is all of the items provided, like ("Name", func).
        """        
        self.menuFunctions = items
        
        self.actor.session.sendPacket(
            0xb7,
            actorID = self.actor.gameID,
            items = ":".join([key for key, val in items])
        )
        
        self.process.kill()

    def next(self, function = None):
        """
        Shows the "Next" button and postpones the script until the player clicks it.
        """
        self.nextFunc = function
        
        self.actor.session.sendPacket(
            0xb5,
            actorID = self.actor.gameID
        )
        
        if function:
            self.process.kill()
        else:
            self.channel.receive()
    
    def end(self):
        """
        Ends the script.
        """
        self.process.kill()
    
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
    
    def cutin(self, filename, position = 2):
        """
        Displays an illustration.
        """
        self.actor.session.sendPacket(
            0x1b3,
            filename = filename,
            position = position
        )
    
    def closeCutin(self, filename):
        """
        Closes the specified cutin.
        """
        self.actor.session.sendPacket(
            0x1b3,
            filename = filename,
            position = 255
        )
    
    def closeCutins(self):
        """
        Closes all cutins.
        """
        self.actor.session.sendPacket(
            0x1b3,
            filename = "",
            position = 255
        )
