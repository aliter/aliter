import thread

environment = {}
    
class Script(object):
    def __init__(self, code, filename = "<string>"):
        self.code = compile(code, filename, "exec")
    
    def run(self, actor):
        self.actor = actor
        thread.start_new_thread(self.threadedRun, ())
    
    def threadedRun(self):
        exec self.code in { "say": self.say, "close": self.close, "menu": self.menu, "next": self.next }
        
    
    def say(self, message):
        self.actor.session.sendPacket(
            0xb4,
            actorID = self.actor.gameID,
            message = str(message) + "\x00"
        )
    
    def close(self):
        self.actor.session.sendPacket(
            0xb6,
            actorID = self.actor.gameID
        )
    
    def menu(self, items):
        self.menu = items
        self.actor.session.sendPacket(
            0xb7,
            actorID = self.actor.gameID,
            items = ":".join(items.keys())
        )

    def next(self, function):
        self.next2 = function
        self.actor.session.sendPacket(
            0xb5,
            actorID = self.actor.gameID
        )
    
    def waiting(self):
        return not self.done_waiting
        