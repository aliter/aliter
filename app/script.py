import thread

environment = {}
    
class Script(object):
    ended = False
    
    def __init__(self, code, filename = "<string>"):
        self.code = compile(code, filename, "exec")
    
    def run(self, actor):
        self.actor = actor
        thread.start_new_thread(self.threadedRun, ())
    
    def threadedRun(self):
        exec self.code in { "say": self.say, "close": self.close, "menu": self.menu, "next": self.next }
        
    
    def say(self, message):
        if self.ended:
            return
        
        self.actor.session.sendPacket(
            0xb4,
            actorID = self.actor.gameID,
            message = str(message) + "\x00"
        )
    
    def close(self):
        if self.ended:
            return
        
        self.actor.session.sendPacket(
            0xb6,
            actorID = self.actor.gameID
        )
        self.ended = True
    
    def menu(self, items):
        if self.ended:
            return
        
        self.menuFunctions = items
        
        self.actor.session.sendPacket(
            0xb7,
            actorID = self.actor.gameID,
            items = ":".join(items.keys())
        )

    def next(self, function = None):
        if self.ended:
            return
        
        self.nextFunc = function
        
        self.actor.session.sendPacket(
            0xb5,
            actorID = self.actor.gameID
        )
        
        self.actor.waitNext = True
        
        while self.actor.waitNext:
            pass
        